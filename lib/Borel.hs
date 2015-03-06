{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}

module Borel
  ( -- * Metric
    Metric(..)
  , cpu, block, ssd, diskReads, diskWrites, neutronIn, neutronOut
  , ipv4, vcpus, memory, snapshot, image
  , mkInstance

    -- * Unit of measurement
  , UOM(..), BaseUOM(..), Prefix(..)
  , nanosec, byte, megabyte, gigabyte
  , countCPU, countVCPU, countInstance, countIP
  , convert
  , nanosecToSec, nanosecToHour, byteToGigabyte

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
  , respResource, respResourceID, respVal
  , uomVal

    -- * Running
  , run, runF
  , BorelError(..)

  ) where

import           Control.Applicative
import           Control.Concurrent.Async
import qualified Control.Exception    as E
import           Control.Lens
import           Control.Monad.Reader
import           Data.Set             (Set)
import qualified Data.Set             as S
import           Pipes                hiding (Proxy)
import qualified Pipes                as P
import           Pipes.Concurrent
import qualified Pipes.Prelude        as P
import           Pipes.Safe
import           System.Log.Logger

-- friends
import           Ceilometer.Client
import           Vaultaire.Types

-- family
import           Borel.Ceilometer
import           Borel.Chevalier
import           Borel.Marquise
import           Borel.Types


--------------------------------------------------------------------------------

-- | Metrics that must be queried as a common Ceilometer resource are grouped together,
--   others are left as-is.
--
--   e.g. [instance-1, instance-2,..]
--
groupMetrics :: Set Metric        -- ^ All available instances
             -> Set Metric        -- ^ Metrics to be grouped
             -> Set GroupedMetric
groupMetrics instances metrics =
    let (allfs, nonfs) = bimap S.toList (S.map pure) $ S.partition (`S.member` instances) metrics
    in case allfs of
        [] -> nonfs
        xs -> S.insert xs nonfs


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

query :: (Applicative m, MonadSafe m, MonadIO m)
      =>  Producer (Metric, ResponseItem) (BorelS m) ()
query = do
  liftIO $ updateGlobalLogger "borel" (setLevel DEBUG)

  params <- ask
  (outputWork, inputWork, sealWork) <- liftIO . spawn' $ unbounded
  (outputRes, inputRes, sealRes) <- liftIO . spawn' $ unbounded
  let grouped = groupMetrics
              ( params ^. paramBorelConfig . allInstances)
              ( params ^. paramMetrics)
      defaultFilters = Filters billableStatus
      producer
          :: (MonadIO m, MonadSafe m)
          => Producer (GroupedMetric, Origin, Address, SourceDict) m ()
      producer = P.enumerate
        [ (metrics, org, addr, sd)
        | metrics         <- Select $ P.each grouped
        , (org, addr, sd) <- Select $ chevalier params (metrics, params ^. paramTID)
        ]
      worker
          :: (MonadIO m, MonadSafe m)
          => Pipe (GroupedMetric, Origin, Address, SourceDict)
                  (Metric, ResponseItem)
                  m
                  ()
      worker = forever $ do
        let flavors = params ^. paramBorelConfig . paramFlavorMap
            start   = params ^. paramStart
            end     = params ^. paramEnd

        (metrics, org, addr, sd) <- await
        results <- ceilometer flavors metrics
              (Env flavors sd defaultFilters start end)
              (marquise params (metrics, org, addr))
        forM_ results $ \result -> yield (fst result, mkItem sd result)

  -- This hierarchy is necessary to close the buffers in case of an error.
  pollWorkers <- liftIO . async $ (do
    workers <- replicateM 8 . async . runSafeT . runEffect $
                 fromInput inputWork >-> worker >-> toOutput outputRes
    mapM_ link workers
    mapM_ wait workers) `E.finally` (atomically $ sealWork >> sealRes)
  liftIO $ link pollWorkers

  pollProducer <- liftIO . async $
    (runSafeT . runEffect $ producer >-> toOutput outputWork)
      `E.finally` atomically sealWork
  liftIO $ link pollProducer

  fromInput inputRes

  liftIO $ wait pollProducer
  liftIO . atomically $ sealRes
  liftIO $ wait pollWorkers
