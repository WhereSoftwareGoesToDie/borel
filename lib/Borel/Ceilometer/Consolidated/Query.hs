{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeOperators    #-}

module Borel.Ceilometer.Consolidated.Query
  ( eventQuery
  , pollsterQuery
  ) where

import           Control.Monad
import           Control.Monad.Logger
import           Control.Monad.Trans.Reader
import           Data.Binary
import           Data.Binary.Get
import           Data.Map                               (Map)
import qualified Data.Map                               as M
import           Data.Word
import           Pipes
import qualified Pipes.Prelude                          as P
import           Pipes.Safe


import           Marquise.Client
import           Marquise.Types
import           Vaultaire.Control.Lift
import           Vaultaire.Query                        hiding (readSimple)
import           Vaultaire.Types

import           Borel.Types
import           Borel.Ceilometer.Consolidated.Parse
import           Borel.Ceilometer.Consolidated.Types
import           Borel.Log

matchPayloadAndResource :: Payload -> Resource -> Bool
matchPayloadAndResource M1Tiny     = (== instanceM1Tiny)
matchPayloadAndResource M1Small    = (== instanceM1Small)
matchPayloadAndResource M1Medium   = (== instanceM1Medium)
matchPayloadAndResource M1Large    = (== instanceM1Large)
matchPayloadAndResource M1XLarge   = (== instanceM1XLarge)
matchPayloadAndResource IPAlloc    = (== ipv4)
matchPayloadAndResource (Volume _) = (== volumes)
matchPayloadAndResource (Memory _) = (== memory)
matchPayloadAndResource (VCpu _)   = (== vcpus)

payloadWeight :: Payload -> Word64
payloadWeight (Volume x) = fromIntegral x
payloadWeight (Memory x) = fromIntegral x
payloadWeight (VCpu   x) = fromIntegral x
payloadWeight _          = 1

summaryToResourceQuery :: (Monad m)
                       => [Resource]
                       -> Map Payload Word64
                       -> Query m (Resource, Word64)
summaryToResourceQuery rs summaryMap = Select $ forM_ rs $ \resource ->
    let filteredMap = M.filterWithKey (\k _ -> matchPayloadAndResource k resource) summaryMap
        summedValue = M.foldlWithKey  (\acc k v -> payloadWeight k * v + acc) 0 filteredMap
    in yield (resource, summedValue)

summarisePollster :: (Monad m)
                  => ResourceGroup
                  -> TimeStamp
                  -> TimeStamp
                  -> Query m ConsolidatedPoint
                  -> m (Map Payload Word64)
summarisePollster rGroup (TimeStamp start) (TimeStamp end) (Select points) = summarise' rGroup Nothing M.empty points start end


-- |Filters out irrelevant events (failures, etc.) and produces a summary
--  in the form of a Map from possible payloads and durations
summariseEvents :: (Monad m)
                => ResourceGroup
                -> TimeStamp
                -> TimeStamp
                -> Query m ConsolidatedPoint
                -> m (Map Payload Word64)
summariseEvents rGroup (TimeStamp start) (TimeStamp end) (Select points) = summarise' rGroup Nothing M.empty (points >-> filterRelevant) start end
  where
    endpointPred evt
        | eventEndpoint evt == Instant = True
        | eventEndpoint evt == End     = True
        | otherwise                    = False
    filterRelevant = P.filter endpointPred

billableEvent rGroup EventPoint{..}    = billableVerb rGroup eventVerb
billableEvent _ PollsterPoint{..} = True

summarise' :: (Monad m)
           => ResourceGroup
           -> Maybe ConsolidatedPoint
           -> Map Payload Word64
           -> Producer ConsolidatedPoint m ()
           -> Word64
           -> Word64
           -> m (Map Payload Word64)
summarise' rGroup lastEvent acc prod start end = do
    either_res <- next prod
    case either_res of
        Left _ -> case lastEvent of
            Nothing -> return M.empty
            Just evt -> do
                let delta = end - extractTime evt
                let acc' = if not (billableEvent rGroup evt) || delta <= 0
                    then
                        acc
                    else
                        M.insertWith (+) (extractPayload evt) delta acc
                return acc'
        Right (currEvent, prod') ->
            case lastEvent of
                Nothing  -> summarise' rGroup (Just currEvent) acc prod' start end
                Just lastEvent' ->
                    if extractTime lastEvent' > end
                    then
                        return acc
                    else if extractTime currEvent < start
                    then
                        summarise' rGroup (Just currEvent) acc prod' start end
                    else do
                        let start' = max start (extractTime lastEvent')
                        let end'   = min end (extractTime currEvent)
                        let delta  = end' - start'
                        let acc'   = M.insertWith (+) (extractPayload lastEvent') delta acc
                        if not (billableEvent rGroup lastEvent') || delta <= 0
                        then
                            return acc
                        else
                            summarise' rGroup (Just currEvent) acc' prod' start end

-- | Runs an aggregation query for this event-based resource.
--   We make the assumption that @r@ is reported by ceilometer events.
eventQuery :: ( MonadSafe m
              , MonadLogger m
              , ReaderT BorelEnv `In` m )
           => ResourceGroup -> [Resource]
           -> Origin -> Address -> TimeStamp -> TimeStamp
           -> Query m (Resource, Word64)
eventQuery rGroup rs o a s e =
  logInfoThen (concat ["Running event query for address ", show a]) $ do
    env <- liftT ask
    let resQuery = parseConsolidated rGroup (eventMetrics (marquiseReader $ config env) o a)
    summary <- lift $ summariseEvents rGroup s e resQuery
    summaryToResourceQuery rs summary

-- | Runs an aggregation query for this pollster-based resource.
--   We make the assumption that @r@ is reported by ceilometer pollsters.
pollsterQuery :: ( MonadSafe m
              , MonadLogger m
              , ReaderT BorelEnv `In` m )
           => ResourceGroup -> [Resource]
           -> Origin -> Address -> TimeStamp -> TimeStamp
           -> Query m (Resource, Word64)
pollsterQuery rGroup rs o a s e =
  logInfoThen (concat ["Running pollster query for address ", show a]) $ do
    env <- liftT ask
    let resQuery = parseConsolidated rGroup (metrics (marquiseReader $ config env) o a s e)
    summary <- lift $ summarisePollster rGroup s e resQuery
    summaryToResourceQuery rs summary
