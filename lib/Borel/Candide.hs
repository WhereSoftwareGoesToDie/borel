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

module Borel.Candide
     ( candide
     , findAddrSd )

where

import           Control.Lens
import           Data.Monoid
import           Data.Pool
import           Data.Set                   (Set)
import qualified Data.Set                   as S
import           Data.Text                  (Text)
import           Database.PostgreSQL.Simple as PG
import           Pipes
import           Pipes.Safe                 as P
import           System.Log.Logger

-- friends
import           Candide.Core
import           Ceilometer.Tags
import           Vaultaire.Types

-- family
import           Borel.Types


-- | Use Candide to fetch raw data points.
--
candide :: (MonadIO m, MonadSafe m)
        => BorelEnv
        -> Pool PG.Connection
        -> (GroupedMetric, Origin, Address)
        -> Producer SimplePoint m ()
candide params connPool (metrics, origin, addr) = do
  let end     = params ^. paramEnd
  let eventStart = TimeStamp 0
  let pointstart = params ^. paramStart
  let start = case metrics of
        [metric] -> if
          | metric == block    -> eventStart
          | metric == ssd      -> eventStart
          | metric == ipv4     -> eventStart
          | metric == snapshot -> eventStart
          | otherwise          -> pointstart
        _                      -> pointstart

  liftIO $ debugM "borel" ("fetching from candide with origin="
                           <> show origin
                           <> " addr="
                           <> show addr)
  res <- liftIO $ withResource connPool $ \conn -> readSimple conn addr start end
  Pipes.each res

--------------------------------------------------------------------------------

findAddrSd :: (MonadIO m, MonadSafe m)
           => BorelEnv
           -> Pool PG.Connection
           -> (GroupedMetric, TenancyID)
           -> Producer (Address, SourceDict) m ()
findAddrSd params connPool (metrics, tid) = do
  let tags = mkTags
             (params ^. paramBorelConfig . allInstances)
             (metrics, tid)
  liftIO $ debugM "borel" ("searching candide with tags=" <> show tags)
  addrSds <- liftIO $ withResource connPool $ \conn -> searchTags conn tags
  Pipes.each addrSds

-- | Converts a GroupedMetric and a TenancyID into a list of cheavlier tags
--   Also requires a set of the currently configured instance flavors
mkTags :: Set Metric -> (GroupedMetric, TenancyID) -> [(Text, Text)]
mkTags instances (ms, TenancyID tid) = case ms of
  [metric] -> if
    | metric == cpu        -> [tagID tid , tagName "cpu"                                  ]
    | metric == block      -> [tagID tid , tagName "volume.size", tagBlock  , tagEvent    ]
    | metric == ssd        -> [tagID tid , tagName "volume.size", tagFast   , tagEvent    ]
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
        tagBlock    = (keyVolumeType, valVolumeBlock)
        tagFast     = (keyVolumeType, valVolumeFast)
