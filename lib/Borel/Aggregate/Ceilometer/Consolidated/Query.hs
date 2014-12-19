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
import           Control.Monad.Trans.Reader
import           Data.Binary
import           Data.Map                                      (Map)
import qualified Data.Map                                      as M
import           Pipes
import qualified Pipes.Prelude                                 as P
import           Pipes.Safe


import           Marquise.Client
import           Vaultaire.Control.Lift
import           Vaultaire.Query

import           Borel.Aggregate.Ceilometer.Consolidated.Parse
import           Borel.Aggregate.Ceilometer.Consolidated.Types
import           Borel.Log
import           Borel.Types


data ConsolidatedType = EventBased | PollsterBased

summarise EventBased    = summariseEvents
summarise PollsterBased = summarisePollster

matchPayloadAndMetric :: FlavorMap -> Payload -> Metric -> Bool
matchPayloadAndMetric fm (ComputeInstance name) = (== computeInstance name)
matchPayloadAndMetric _  IPAlloc                = (== ipv4)
matchPayloadAndMetric _  (Volume _)             = (== volumes)
matchPayloadAndMetric _  (Memory _)             = (== memory)
matchPayloadAndMetric _  (VCpu _)               = (== vcpus)
matchPayloadAndMetric _  _                      = const False

payloadWeight :: Payload -> Word64
payloadWeight (Volume x) = fromIntegral x
payloadWeight (Memory x) = fromIntegral x
payloadWeight (VCpu   x) = fromIntegral x
payloadWeight _          = 1

summaryToMetricQuery :: (Monad m)
                     => FlavorMap
                     -> [Metric]
                     -> Map Payload Word64
                     -> Query m (Metric, Word64)
summaryToMetricQuery fm rs summaryMap = Select $ forM_ rs $ \resource ->
    let filteredMap = M.filterWithKey (\k _ -> matchPayloadAndMetric fm k resource) summaryMap
        summedValue = M.foldlWithKey  (\acc k v -> payloadWeight k * v + acc) 0 filteredMap
    in yield (resource, summedValue)

summarisePollster :: (Monad m)
                  => MetricGroup
                  -> TimeStamp
                  -> TimeStamp
                  -> Query m ConsolidatedPoint
                  -> m (Map Payload Word64)
summarisePollster rGroup (TimeStamp start) (TimeStamp end) (Select points) = summarise' rGroup Nothing M.empty points start end False


-- |Filters out irrelevant events (failures, etc.) and produces a summary
--  in the form of a Map from possible payloads and durations
summariseEvents :: (Monad m)
                => MetricGroup
                -> TimeStamp
                -> TimeStamp
                -> Query m ConsolidatedPoint
                -> m (Map Payload Word64)
summariseEvents rGroup (TimeStamp start) (TimeStamp end) (Select points) = summarise' rGroup Nothing M.empty (points >-> filterRelevant) start end True
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
           -> Bool
           -> m (Map Payload Word64)
summarise' rGroup lastEvent acc prod start end isEvent = do
    either_res <- next prod
    case either_res of
        Left _ -> case lastEvent of
            Nothing -> return M.empty
            Just evt -> if isEvent then do
                let delta = end - extractTime evt
                let acc' = if not (billableEvent rGroup evt) || delta <= 0
                    then
                        acc
                    else
                        M.insertWith (+) (extractPayload evt) delta acc
                return acc'
                else return acc
        Right (currEvent, prod') ->
            case lastEvent of
                Nothing  -> summarise' rGroup (Just currEvent) acc prod' start end isEvent
                Just lastEvent' ->
                    if extractTime lastEvent' > end
                    then
                        return acc
                    else if extractTime currEvent < start
                    then
                        summarise' rGroup (Just currEvent) acc prod' start end isEvent
                    else do
                        let start' = max start (extractTime lastEvent')
                        let end'   = min end (extractTime currEvent)
                        let delta  = end' - start'
                        let acc'   = M.insertWith (+) (extractPayload lastEvent') delta acc
                        if not (billableEvent rGroup lastEvent') || delta <= 0
                        then
                            return acc
                        else
                            summarise' rGroup (Just currEvent) acc' prod' start end isEvent

aggregateConsolidated
  :: ( ReaderT BorelEnv `In` m
     , MonadSafe m
     , MonadLogger m )
  => ConsolidatedType
  -> MetricGroup
  -> [Metric]
  -> TimeStamp -> TimeStamp
  -> Query m SimplePoint
  -> Query m (Metric, Word64)
aggregateConsolidated contype group ms s e source = do
  BorelEnv _ fm _ _ <- liftT ask
  logInfoThen "Running consolidated query" $ do
    let resQuery = parseConsolidated fm group source
    summary <- lift $ summarise contype group s e resQuery
    summaryToMetricQuery fm ms summary
