{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TransformListComp   #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE RecordWildCards     #-}

-- | Evaluates Borel requests and construct a Borel queries for them.
--
module Borel.Query
  ( mkQuery
  ) where

import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.List (nub)
import           Data.Monoid
import           Data.Word
import           Pipes
import qualified Pipes.Prelude as P
import           Pipes.Safe

-- friends
import           Vaultaire.Types
import           Vaultaire.Query hiding (metrics)

-- family
import           Borel.Types
import           Borel.Log
import           Borel.Aggregate

-- | Construct a Borel aggregation from a source of raw Vaultaire points
--
mkQuery :: ( MonadReader BorelEnv m
           , MonadSafe m
           , MonadLogger m )
      => [Metric]
      -> TimeStamp -> TimeStamp
      -> Query m SimplePoint
      -> Query m (Metric, Word64)
mkQuery metrics start end source =
  maybe (lift (logWarnStr "mkQuery: the requested metrics must be of the same group") >> mzero)
        (\group -> logInfoThen
          ("Aggregating data for resources " <> (show $ map deserialise metrics))
          $ case report group of
               Delta                -> processNonConsolidated (aggregateDelta source)
               Cumulative           -> processNonConsolidated (aggregateCumulative source)
               ConsolidatedPollster -> aggregateConsolidated PollsterBased group metrics start end source
               ConsolidatedEvent    -> aggregateConsolidated EventBased    group metrics start end source )
        (groupOf metrics)
  where pair (Select vals) metric = Select (vals >-> P.map (metric,))
        processNonConsolidated q = case metrics of
          [resource] -> pair q resource --Currently all non-consolidated metrics have a 1:1 resource:group relation
          _          -> mzero

groupOf :: [Metric] -> Maybe MetricGroup
groupOf xs = case nub (map group xs) of
  x:[] -> Just x
  _    -> Nothing
