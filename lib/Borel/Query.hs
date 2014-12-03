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
import           Control.Monad.Trans.Reader
import           Data.Word
import           Pipes
import qualified Pipes.Prelude as P
import           Pipes.Safe

-- friends
import           Vaultaire.Types
import           Vaultaire.Query
import           Vaultaire.Control.Lift

-- family
import           Borel.Types
import           Borel.Log
import           Borel.Ceilometer

-- | Construct a Borel query
--
mkQuery :: ( ReaderT BorelEnv `In` m
         , MonadSafe m
         , MonadLogger m )
      => Origin -> Address -> TimeStamp -> TimeStamp
      -> [Metric] -> Query m (Metric, Word64)
mkQuery _   _    _     _    []    = Select $ return ()
mkQuery org addr start end rs@(r:rs') = do
    let rGroup = group r
    if all (\r' -> group r' == rGroup) rs' then
      logInfoThen (concat [ "Aggregating data from ", show addr
                          , " for resources "       , show $ map deserialise rs
                          ]) $
      case report rGroup of
          Delta                -> processNonConsolidated (gaugeQuery org addr start end)
          Cumulative           -> processNonConsolidated (cumulativeQuery org addr start end)
          ConsolidatedPollster -> pollsterQuery rGroup rs org addr start end
          ConsolidatedEvent    -> eventQuery rGroup rs org addr start end
    else do
      lift $ logWarnStr "query passed resource list with non-matching resourceGroups, returning empty Query)"
      Select $ return ()
  where pair (Select points) resource = Select (points >-> P.map (\p -> (resource, p)))
        processNonConsolidated q = case rs of
          [resource] -> pair q resource --Currently all non-consolidated metrics have a 1:1 resource:group relation
          _          -> Select $ return ()
