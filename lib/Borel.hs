{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}

module Borel
  ( run
  , module Borel.Types
  ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Reader
import qualified Data.Bimap           as BM
import qualified Data.Foldable        as F
import qualified Data.List            as L
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Maybe
import           Data.Monoid
import           Data.Text            (Text)
import           Data.Word
import           Pipes                hiding (Proxy)
import qualified Pipes                as P
import           Pipes.Safe

-- friends
import           Ceilometer.Client
import qualified Chevalier.Util       as C
import           Vaultaire.Query
import           Vaultaire.Types

-- family
import           Borel.Types


type Result = (Metric, Word64)

-- | Leverages Chevalier, Marquise and Ceilometer
--   to find, fetch, decode and aggregate data for an OpenStack tenancy.
--
run :: (MonadSafe m, Applicative m)
    => Config               -- ^ Borel config, e.g. contains Chevalier/Marquise URI.
    -> [Metric]             -- ^ Metrics requested
    -> TenancyID            -- ^ OpenStack tenancy (can lead to multiple metrics)
    -> TimeStamp            -- ^ Start time
    -> TimeStamp            -- ^ End time
    -> Producer Result m ()
run conf ms tid s e = runBorel conf ms tid s e query

query :: (Applicative m, MonadSafe m)
      => Producer Result (BorelM m) ()
query = do
  params <- ask
  P.enumerate
    [ result
    | (org, addr, sd) <- Select $ chevalier (params ^. paramTID)
    , result          <- Select $ each' $ go
                         (params ^. paramMetrics)
                         (params ^. paramFlavorMap)
                         (ceilometer params sd)
                         (marquise   params org addr)
    ]
  where ceilometer params sd = Env (params ^. paramFlavorMap) sd
                                   (params ^. paramStart)
                                   (params ^. paramEnd)

        each' :: Monad m => m [a] -> Producer a m ()
        each' x = lift x >>= P.each

-- | Use Ceilometer to decode and aggregate a stream of raw data points.
--
go :: (Monad m, Applicative m)
    => [Metric]                  -- ^ Requested metrics, this determines how we present the fold result
    -> FlavorMap                 -- ^ Instance flavor mapping
    -> Env                       -- ^ Ceilometer arguments
    -> Producer SimplePoint m () -- ^ Raw points
    -> m [Result]
go metrics fm ceilometer points = case metrics of
  [m] -> if
    | m == cpu     -> poke (single m) $ decodeAndFold (undefined :: proxy PDCPU)            ceilometer points
    | m == volumes -> poke (single m) $ decodeAndFold (undefined :: proxy PDVolume)         ceilometer points
    | m == vcpus   -> poke (sumMap m) $ decodeAndFold (undefined :: proxy PDInstanceVCPU)   ceilometer points
    | otherwise    -> return []
  ms@_   -> if
    -- the flavors queried are known in our flavor map config.
    | fs <- intersect ms fm, not $ null fs
                   -> poke (group fs) $ decodeAndFold (undefined :: proxy PDInstanceFlavor) ceilometer points
    -- we have no idea what they're talking about!
    | otherwise    -> return []
  where single     :: Metric -> Word64 -> [Result]
        single m v =  [(m, v)]

        sumMap      :: Metric -> Map PFValue32 Word64 -> [Result]
        sumMap m vs = [(m, M.foldlWithKey (\a k v -> a + (fromIntegral k * v)) 0 vs)]

        group      :: [(Metric, Flavor)] -> Map PFValueText Word64 -> [Result]
        group ms vs = map (\(metric, flavor) -> (metric,) $ fromMaybe 0 $ M.lookup flavor vs) ms

        poke :: (Functor f, Foldable t, Monoid b) => (a -> b) -> f (t a) -> f b
        poke = fmap . F.foldMap

        --  Intersect the flavor map and the list of metrics requested. Flatten the result.
        intersect :: [Metric] -> FlavorMap -> [(Metric, Flavor)]
        intersect ms = BM.fold (\k1 _ acc -> maybe acc ((:acc) . (,k1))
                                          $  L.find (== mkInstance k1) ms) []


-- | Use Marquise to fetch raw data points.
--
marquise :: MonadSafe m
         => BorelEnv
         -> Origin
         -> Address
         -> Producer SimplePoint m ()
marquise params origin addr = case params ^. paramMetrics of
  [metric] -> if
    | metric == volumes  -> getAllPoints
    | metric == ipv4     -> getAllPoints
    | metric == snapshot -> getAllPoints
    | otherwise          -> getSomePoints
  _                      -> getSomePoints
  where getAllPoints = P.enumerate $ eventMetrics
                         (params ^. paramMarquiseURI)
                          origin addr
                         (params ^. paramStart)
                         (params ^. paramEnd)
        getSomePoints = P.enumerate $ getMetrics
                         (params ^. paramMarquiseURI)
                          origin addr
                         (params ^. paramStart)
                         (params ^. paramEnd)

-- | Use Chevalier to find origin, address, sourcedict that contains data relevant
--   to this OpenStack tenancy.
--
chevalier :: MonadSafe m
          => TenancyID
          -> Producer (Origin, Address, SourceDict) (BorelM m) ()
chevalier tid = do
  params <- lift ask
  let req = C.buildRequestFromPairs $ chevalierTags
            (params ^. paramFlavorMap)
            (params ^. paramMetrics)
             tid
  P.enumerate
    [ (org, addr, sd)
    | org        <- Select $ P.each (params ^. paramOrigin)
    , (addr, sd) <- addressesWith ( params ^. paramChevalierURI) org req
    ]

chevalierTags :: FlavorMap -> [Metric] -> TenancyID -> [(Text, Text)]
chevalierTags fm ms tid = case ms of
  [metric] -> if
    | metric == cpu        -> [tagID tid , tagName "cpu"                                  ]
    | metric == volumes    -> [tagID tid , tagName "volume.size"            , tagEvent    ]
    | metric == diskReads  -> [tagID tid , tagName "disk.read.bytes"                      ]
    | metric == diskWrites -> [tagID tid , tagName "disk.write.bytes"                     ]
    | metric == neutronIn  -> [tagID tid , tagName "network.incoming.bytes"               ]
    | metric == neutronOut -> [tagID tid , tagName "network.outgoing.bytes"               ]
    | metric == ipv4       -> [tagID tid , tagName "ip.floating"            , tagEvent    ]
    | metric == vcpus      -> [tagID tid , tagName "instance_vcpus"         , tagPollster ]
    | metric == memory     -> [tagID tid , tagName "instance_ram"           , tagPollster ]
    | metric == snapshot   -> [tagID tid , tagName "snapshot.size"          , tagEvent    ]
    | metric == image      -> [tagID tid , tagName "image.size"             , tagPollster ]
    | otherwise            -> []
  _ -> if
    | all (`elem` allInstances fm) ms
                           -> [tagID tid , tagName "instance_flavor"        , tagPollster ]
    | otherwise            -> []
  where tagID       = ("project_id",)
        tagName     = ("metric_name",)
        tagEvent    = ("_event", "1")
        tagPollster = ("_event", "0")
