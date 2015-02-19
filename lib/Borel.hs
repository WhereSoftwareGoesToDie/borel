{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE CPP                 #-}

module Borel
  ( -- * Metric
    Metric(..)
  , cpu, volumes, diskReads, diskWrites, neutronIn, neutronOut
  , ipv4, vcpus, memory, snapshot, image
  , mkInstance

    -- * Unit of measurement
  , UOM(..), BaseUOM(..), Prefix(..)
  , nanosec, byte, megabyte, gigabyte
  , convert
  , nanosecToSec, byteToGigabyte

    -- * BorelConfig
  , BorelConfig(..)
  , mkBorelConfig, parseBorelConfig, loadBorelConfig
  , allInstances, allMetrics
  , paramBorelConfig, paramFlavorMap, paramOrigins, paramMarquiseURI, paramChevalierURI

    -- * Query arguments
  , BorelEnv
  , paramStart, paramEnd, paramMetrics, paramTID
  , TenancyID(..)

    -- * Query results
  , ResponseItem(..)
  , mkItem
  , respResource, respResourceID, respUOM, respQuantity
  , traverseUOMVal

    -- * Running
  , run, runF
  , BorelError(..)

  ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Reader
import qualified Data.Bimap           as BM
import qualified Data.List            as L
import qualified Data.Map             as M
import           Data.Maybe
import           Data.Set             (Set)
import qualified Data.Set             as S
import           Data.Text            (Text)
import           Pipes                hiding (Proxy)
import qualified Pipes                as P
import qualified Pipes.Prelude                as P
import           Pipes.Safe

#ifdef BOREL_DEBUG
import           Data.Monoid
import           System.Log.Logger
#endif

-- friends
import           Ceilometer.Client
import           Ceilometer.Tags
import qualified Chevalier.Util       as C
import           Vaultaire.Query
import           Vaultaire.Types

-- family
import           Borel.Types


--------------------------------------------------------------------------------

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


--------------------------------------------------------------------------------

-- | Leverages Chevalier, Marquise and Ceilometer
--   to find, fetch, decode and aggregate data for an OpenStack tenancy.
--
run :: (MonadSafe m, Applicative m)
    => BorelConfig          -- ^ Borel config, e.g. contains Chevalier/Marquise URI.
    -> Set Metric           -- ^ Metrics requested
    -> TenancyID            -- ^ OpenStack tenancy (can lead to multiple metrics)
    -> TimeStamp            -- ^ Start time
    -> TimeStamp            -- ^ End time
    -> Producer ResponseItem m ()
run    conf ms tid s e = runBorel conf ms tid s e (query >-> P.map snd)

-- | Acts like a functor over the produced results.
--   This is not a Haskell functor, it's a functor between Hask and cat of Proxy composition.
--
runF :: (MonadSafe m, Applicative m)
     => ((Metric, ResponseItem) -> a)
     -> BorelConfig          -- ^ Borel config, e.g. contains Chevalier/Marquise URI.
     -> Set Metric           -- ^ Metrics requested
     -> TenancyID            -- ^ OpenStack tenancy (can lead to multiple metrics)
     -> TimeStamp            -- ^ Start time
     -> TimeStamp            -- ^ End time
     -> Producer a m ()
runF f conf ms tid s e = runBorel conf ms tid s e (query >-> P.map f)

#ifdef BOREL_DEBUG
query :: (Applicative m, MonadSafe m, MonadIO m)
#else
query :: (Applicative m, MonadSafe m)
#endif
      =>  Producer (Metric, ResponseItem) (BorelS m) ()
query = do
#ifdef BOREL_DEBUG
  liftIO $ do
    debugM "borel" "start a Borel query"
    updateGlobalLogger "borel" (setLevel DEBUG)
#endif
  params <- ask
  let flavors = params ^. paramBorelConfig . paramFlavorMap
      start   = params ^. paramStart
      end     = params ^. paramEnd
      grouped = groupMetrics
              ( params ^. paramBorelConfig . allInstances)
              ( params ^. paramMetrics)
  P.enumerate
    [ (fst result, mkItem sd result)
    | metrics         <- Select $ P.each grouped
    , (org, addr, sd) <- Select $ chevalier (metrics, params ^. paramTID)
    , result          <- Select $ each' $ ceilometer
                         flavors metrics
                         (Env flavors sd start end)
                         (marquise params (metrics, org, addr))
    ]
  where each' :: Monad m => m [a] -> Producer a m ()
        each' x = lift x >>= P.each


--------------------------------------------------------------------------------

-- | Use Ceilometer to decode and aggregate a stream of raw data points.
--
#ifdef BOREL_DEBUG
ceilometer
    :: (MonadIO m, Applicative m)
#else
ceilometer
    :: (Monad m, Applicative m)
#endif
    => FlavorMap                 -- ^ Instance flavor mapping
    -> GroupedMetric             -- ^ Requested metrics, this determines how we present the fold result
    -> Env                       -- ^ Ceilometer arguments
    -> Producer SimplePoint m () -- ^ Raw points
    -> m [Result]
ceilometer flavors metrics cenv raw
  =   fromMaybe []
  <$> fmap handleResult
  <$> decodeFold cenv raw
  where handleResult :: FoldResult -> [Result]
        handleResult x = case (x, metrics) of
          (RSingle  val,  [m]) -> [(m,val)]
          (RMapNum  vals, [m]) -> [(m, M.foldlWithKey (\a k v -> a + (fromIntegral k * v)) 0 vals)]
          (RMapText vals,  _)  -> let ms = intersect metrics flavors
                                  in  map (\(metric, flavor) -> (metric,) $ fromMaybe 0 $ M.lookup flavor vals) ms
          _                    -> []

        --  Intersect the flavor map and the list of metrics requested. Flatten the result.
        intersect :: [Metric] -> FlavorMap -> [(Metric, Flavor)]
        intersect ms = BM.fold (\k1 _ acc -> maybe acc ((:acc) . (,k1))
                                          $  L.find (== mkInstance k1) ms) []


--------------------------------------------------------------------------------

-- | Use Marquise to fetch raw data points.
--
#ifdef BOREL_DEBUG
marquise :: (MonadIO m, MonadSafe m)
#else
marquise :: MonadSafe m
#endif
         => BorelEnv
         -> (GroupedMetric, Origin, Address)
         -> Producer SimplePoint m ()
marquise params (metrics, origin, addr) = do
#ifdef BOREL_DEBUG
  liftIO $ debugM "borel" ("fetching from marquise with origin="
                           <> show origin
                           <> " addr="
                           <> show addr)
#endif
  case metrics of
      [metric] -> if
        | metric == volumes  -> getAllPoints
        | metric == ipv4     -> getAllPoints
        | metric == snapshot -> getAllPoints
        | otherwise          -> getSomePoints
      _                      -> getSomePoints
  where getAllPoints = P.enumerate $ eventMetrics
                         (params ^. paramBorelConfig . paramMarquiseURI)
                          origin addr
                         (params ^. paramStart)
                         (params ^. paramEnd)
        getSomePoints = P.enumerate $ getMetrics
                         (params ^. paramBorelConfig . paramMarquiseURI)
                          origin addr
                         (params ^. paramStart)
                         (params ^. paramEnd)

--------------------------------------------------------------------------------

-- | Use Chevalier to find origin, address, sourcedict that contains data relevant
--   to this OpenStack tenancy.
--
#ifdef BOREL_DEBUG
chevalier :: (MonadIO m, MonadSafe m)
#else
chevalier :: MonadSafe m
#endif
          => (GroupedMetric, TenancyID)
          -> Producer (Origin, Address, SourceDict) (BorelS m) ()
chevalier (metrics, tid) = do
#ifdef BOREL_DEBUG
  liftIO $ debugM "borel" ("searching chevalier with tenancy="
                          <> show tid
                          <> " requested="
                          <> show (map deserialise metrics))
#endif
  params <- lift ask
  let req = C.buildRequestFromPairs $ chevalierTags
            (params ^. paramBorelConfig . allInstances)
            (metrics, tid)
  P.enumerate
    [ (org, addr, sd)
    | org        <- Select $ P.each (params ^. paramBorelConfig . paramOrigins)
    , (addr, sd) <- addressesWith (  params ^. paramBorelConfig . paramChevalierURI) org req
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
