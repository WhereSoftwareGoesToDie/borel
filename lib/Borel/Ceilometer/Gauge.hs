{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
module Borel.Ceilometer.Gauge
       ( gaugeQuery )
where

import           Control.Monad.Logger
import           Control.Monad.Trans.Reader
import           Data.Word
import           Pipes.Safe

import           Marquise.Client
import           Vaultaire.Control.Lift
import           Vaultaire.Query

import           Borel.Types
import           Borel.Log

gaugeQuery :: (MonadSafe m, MonadLogger m, ReaderT BorelEnv `In` m)
           => Origin -> Address -> TimeStamp -> TimeStamp -> Query m Word64
gaugeQuery o a s e =
  logInfoThen (concat ["Running gauge query for address ", show a]) $ do
    env <- liftT ask
    sumPoints $ metrics (_readerURI $ config env) o a s e