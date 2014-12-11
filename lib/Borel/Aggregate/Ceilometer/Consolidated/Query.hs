{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeOperators    #-}

module Borel.Aggregate.Ceilometer.Consolidated.Query
  ( aggregateConsolidated
  , ConsolidatedType(..)
  ) where

import           Control.Monad
import           Control.Monad.Logger
import           Data.Binary
import           Data.Map                                      (Map)
import qualified Data.Map                                      as M
import           Pipes
import qualified Pipes.Prelude                                 as P
import           Pipes.Safe


import           Marquise.Client
import           Vaultaire.Query

import           Borel.Aggregate.Ceilometer.Consolidated.Parse
import           Borel.Aggregate.Ceilometer.Consolidated.Types
import           Borel.Log
import           Borel.Types


data ConsolidatedType = EventBased | PollsterBased

summarise EventBased    = summariseEvents
summarise PollsterBased = summarisePollster

matchPayloadAndMetric :: Payload -> Metric -> Bool
matchPayloadAndMetric M1Tiny     = (== instanceM1Tiny)
matchPayloadAndMetric M1Small    = (== instanceM1Small)
matchPayloadAndMetric M1Medium   = (== instanceM1Medium)
matchPayloadAndMetric M1Large    = (== instanceM1Large)
matchPayloadAndMetric M1XLarge   = (== instanceM1XLarge)
matchPayloadAndMetric IPAlloc    = (== ipv4)
matchPayloadAndMetric (Volume _) = (== volumes)
matchPayloadAndMetric (Memory _) = (== memory)
matchPayloadAndMetric (VCpu _)   = (== vcpus)
matchPayloadAndMetric _          = const False

payloadWeight :: Payload -> Word64
payloadWeight (Volume x) = fromIntegral x
payloadWeight (Memory x) = fromIntegral x
payloadWeight (VCpu   x) = fromIntegral x
payloadWeight _          = 1

summaryToMetricQuery :: (Monad m)
                       => [Metric]
                       -> Map Payload Word64
                       -> Query m (Metric, Word64)
summaryToMetricQuery rs summaryMap = Select $ forM_ rs $ \resource ->
    let filteredMap = M.filterWithKey (\k _ -> matchPayloadAndMetric k resource) summaryMap
        summedValue = M.foldlWithKey  (\acc k v -> payloadWeight k * v + acc) 0 filteredMap
    in yield (resource, summedValue)

summarisePollster :: (Monad m)
                  => MetricGroup
                  -> TimeStamp
                  -> TimeStamp
                  -> Query m ConsolidatedPoint
                  -> m (Map Payload Word64)
summarisePollster rGroup (TimeStamp start) (TimeStamp end) (Select points) = summarise' rGroup Nothing M.empty points start end


-- |Filters out irrelevant events (failures, etc.) and produces a summary
--  in the form of a Map from possible payloads and durations
summariseEvents :: (Monad m)
                => MetricGroup
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

billableEvent :: MetricGroup -> ConsolidatedPoint -> Bool
billableEvent rGroup EventPoint{..}    = billableVerb rGroup eventVerb
billableEvent _ PollsterPoint{..} = True

summarise' :: (Monad m)
           => MetricGroup
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

aggregateConsolidated
  :: ( MonadSafe m, MonadLogger m )
  => ConsolidatedType
  -> MetricGroup
  -> [Metric]
  -> TimeStamp -> TimeStamp
  -> Query m SimplePoint
  -> Query m (Metric, Word64)
aggregateConsolidated contype group ms s e source =
  logInfoThen "Running consolidated query" $ do
    let resQuery = parseConsolidated group source
    summary <- lift $ summarise contype group s e resQuery
    summaryToMetricQuery ms summary
