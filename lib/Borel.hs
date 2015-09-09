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
  , paramBorelConfig, paramFlavorMap, paramOrigin

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
import qualified Control.Exception          as E
import           Control.Lens
import           Control.Monad.Reader
import           Data.Monoid
import           Data.Pool
import           Data.Set                   (Set)
import qualified Data.Set                   as S
import           Database.PostgreSQL.Simple as PG hiding (query)
import           Pipes                      hiding (Proxy)
import qualified Pipes                      as P
import           Pipes.Concurrent
import qualified Pipes.Prelude              as P
import           Pipes.Safe
import           System.Log.Logger

-- friends
import           Candide.Core
import           Ceilometer.Client

-- family
import           Borel.Candide
import           Borel.Ceilometer
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

  let conf = params ^. paramBorelConfig
      host = conf ^. paramCandideHost
      port = conf ^. paramCandidePort
      user = conf ^. paramCandideUser
      pass = conf ^. paramCandidePass
      org  = conf ^. paramOrigin
  liftIO $ debugM "borel" ("setting up candide connection to "
                           <> host <> ":" <> show port)

  connPool <- liftIO $ createPool
                         (candideConnection host port user pass (Just org))
                         PG.close --close postgres connection
                         1        --one stripe
                         5        --keep unused open for 5 seconds
                         10       --max connection count of 10

  (outputWork, inputWork, sealWork) <- liftIO . spawn' $ bounded 1
  (outputRes, inputRes, sealRes) <- liftIO . spawn' $ bounded 1
  let grouped = groupMetrics
              ( params ^. paramBorelConfig . allInstances)
              ( params ^. paramMetrics)
      defaultFilters = Filters billableStatus
      producer
          :: (MonadIO m, MonadSafe m)
          => Producer (GroupedMetric, Address, SourceDict) m ()
      producer = P.enumerate
        [ (metrics, addr, sd)
        | metrics         <- Select $ P.each grouped
        , (addr, sd) <- Select $ findAddrSd params connPool (metrics, params ^. paramTID)
        ]
      worker
          :: (MonadIO m, MonadSafe m)
          => Pipe (GroupedMetric, Address, SourceDict)
                  (Metric, ResponseItem)
                  m
                  ()
      worker = forever $ do
        let flavors = params ^. paramBorelConfig . paramFlavorMap
            start   = params ^. paramStart
            end     = params ^. paramEnd

        (metrics, addr, sd) <- await

        results <- ceilometer flavors metrics
              (Env flavors sd defaultFilters start end)
              (candide params connPool (metrics, org, addr))
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
