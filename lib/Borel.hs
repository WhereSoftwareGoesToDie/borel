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
  , cpu, volumes, diskReads, diskWrites, neutronIn, neutronOut
  , ipv4, vcpus, memory, snapshot, image
  , mkInstance

    -- * Unit of measurement
  , UOM(..), BaseUOM(..), Prefix(..)
  , nanosec, byte, megabyte, gigabyte
  , countCPU, countVCPU, countInstance, countIP
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
  , setUOMVal

    -- * Running
  , run, runF
  , BorelError(..)

  ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Reader
import           Data.Set             (Set)
import qualified Data.Set             as S
import           Pipes                hiding (Proxy)
import qualified Pipes                as P
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

query :: (Applicative m, MonadSafe m, MonadIO m)
      =>  Producer (Metric, ResponseItem) (BorelS m) ()
query = do
  liftIO $ updateGlobalLogger "borel" (setLevel DEBUG)

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

