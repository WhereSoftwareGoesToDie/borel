{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TransformListComp #-}

module Borel
  ( -- * Metric
    Metric(..)
  , cpu, volumes, diskReads, diskWrites, neutronIn, neutronOut
  , ipv4, vcpus, memory, snapshot, image
  , mkInstance

    -- * Unit of measurement
  , UOM(..), BaseUOM(..), Prefix(..)
  , nanoSec, byte, megabyte, gigabyte
  , convert

    -- * Config
  , Config(..), mkConfig
  , allInstances, allMetrics
  , paramConfig, paramFlavorMap, paramOrigins, paramMarquiseURI, paramChevalierURI

    -- * Query arguments
  , BorelEnv
  , paramStart, paramEnd, paramMetrics, paramTID
  , TenancyID

    -- * Running
  , ResponseItem(..), mkItem
  , run, runF

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
import           Data.Set             (Set)
import qualified Data.Set             as S
import           Data.Text            (Text)
import           Data.Word
import           Pipes                hiding (Proxy)
import qualified Pipes                as P
import           Pipes.Safe

-- friends
import           Ceilometer.Client
import           Ceilometer.Tags
import qualified Chevalier.Util       as C
import           Vaultaire.Query
import           Vaultaire.Types

-- family
import           Borel.Types


type GroupedMetric = [Metric]

-- | Metrics that must be queried as a common Ceilometer resource are grouped together,
--   others are left as-is.
--
--   e.g. [instance-1, instance-2,..]
--
groupMetrics :: Set Metric        -- ^ All available instances
             -> Set Metric        -- ^ Metrics to be grouped
             -> Set GroupedMetric
groupMetrics instances metrics
  =  let (allfs, nonfs) = _1 %~ S.toList $ S.partition (`S.member` instances) metrics
     in  S.insert allfs $ S.map pure nonfs

-- | Leverages Chevalier, Marquise and Ceilometer
--   to find, fetch, decode and aggregate data for an OpenStack tenancy.
--
run :: (MonadSafe m, Applicative m)
    => Config               -- ^ Borel config, e.g. contains Chevalier/Marquise URI.
    -> Set Metric           -- ^ Metrics requested
    -> TenancyID            -- ^ OpenStack tenancy (can lead to multiple metrics)
    -> TimeStamp            -- ^ Start time
    -> TimeStamp            -- ^ End time
    -> Producer ResponseItem m ()
run    conf ms tid s e = runBorel conf ms tid s e (queryF snd)

runF :: (MonadSafe m, Applicative m)
     => ((Metric, ResponseItem) -> a)
     -> Config               -- ^ Borel config, e.g. contains Chevalier/Marquise URI.
     -> Set Metric           -- ^ Metrics requested
     -> TenancyID            -- ^ OpenStack tenancy (can lead to multiple metrics)
     -> TimeStamp            -- ^ Start time
     -> TimeStamp            -- ^ End time
     -> Producer a m ()
runF f conf ms tid s e = runBorel conf ms tid s e (queryF f)

queryF :: (Applicative m, MonadSafe m)
       => ((Metric, ResponseItem) -> a)
       ->  Producer a (BorelS m) ()
queryF f = do
  params <- ask
  let flavors = params ^. paramConfig . paramFlavorMap
      start   = params ^. paramStart
      end     = params ^. paramEnd
      grouped = groupMetrics
              ( params ^. paramConfig . allInstances)
              ( params ^. paramMetrics)
  P.enumerate
    [ f (fst result, mkItem sd result)
    | metrics         <- Select $ P.each grouped
    , (org, addr, sd) <- Select $ chevalier (metrics, params ^. paramTID)
    , result          <- Select $ each' $ ceilometer
                         flavors metrics
                         (Env flavors sd start end)
                         (marquise params (metrics, org, addr))
    ]
  where each' :: Monad m => m [a] -> Producer a m ()
        each' x = lift x >>= P.each

-- | Use Ceilometer to decode and aggregate a stream of raw data points.
--
ceilometer
    :: (Monad m, Applicative m)
    => FlavorMap                 -- ^ Instance flavor mapping
    -> GroupedMetric             -- ^ Requested metrics, this determines how we present the fold result
    -> Env                       -- ^ Ceilometer arguments
    -> Producer SimplePoint m () -- ^ Raw points
    -> m [Result]
ceilometer fm metrics cenv points = case metrics of
  [m] -> if
    | m == cpu     -> poke (single m) $ decodeAndFold (undefined :: proxy PDCPU)            cenv points
    | m == volumes -> poke (single m) $ decodeAndFold (undefined :: proxy PDVolume)         cenv points
    | m == vcpus   -> poke (sumMap m) $ decodeAndFold (undefined :: proxy PDInstanceVCPU)   cenv points
    | otherwise    -> return []
  ms@_   -> if
    -- the flavors queried are known in our flavor map config.
    | fs <- intersect ms fm, not $ null fs
                   -> poke (group fs) $ decodeAndFold (undefined :: proxy PDInstanceFlavor) cenv points
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
         -> (GroupedMetric, Origin, Address)
         -> Producer SimplePoint m ()
marquise params (metrics, origin, addr) = case metrics of
  [metric] -> if
    | metric == volumes  -> getAllPoints
    | metric == ipv4     -> getAllPoints
    | metric == snapshot -> getAllPoints
    | otherwise          -> getSomePoints
  _                      -> getSomePoints
  where getAllPoints = P.enumerate $ eventMetrics
                         (params ^. paramConfig . paramMarquiseURI)
                          origin addr
                         (params ^. paramStart)
                         (params ^. paramEnd)
        getSomePoints = P.enumerate $ getMetrics
                         (params ^. paramConfig . paramMarquiseURI)
                          origin addr
                         (params ^. paramStart)
                         (params ^. paramEnd)

-- | Use Chevalier to find origin, address, sourcedict that contains data relevant
--   to this OpenStack tenancy.
--
chevalier :: MonadSafe m
          => (GroupedMetric, TenancyID)
          -> Producer (Origin, Address, SourceDict) (BorelS m) ()
chevalier (metrics, tid) = do
  params <- lift ask
  let req = C.buildRequestFromPairs $ chevalierTags
            (params ^. paramConfig . allInstances)
            (metrics, tid)
  P.enumerate
    [ (org, addr, sd)
    | org        <- Select $ P.each (params ^. paramConfig . paramOrigins)
    , (addr, sd) <- addressesWith (  params ^. paramConfig . paramChevalierURI) org req
    ]

chevalierTags :: Set Metric -> (GroupedMetric, TenancyID) -> [(Text, Text)]
chevalierTags instances (ms, TenancyID tid) = case ms of
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
    | all (`S.member` instances) ms
                           -> [tagID tid , tagName "instance_flavor"        , tagPollster ]
    | otherwise            -> []
  where tagID       = (keyTenancyID,)
        tagName     = (keyMetricName,)
        tagEvent    = (keyEvent, valTrue)
        tagPollster = (keyEvent, valFalse)
