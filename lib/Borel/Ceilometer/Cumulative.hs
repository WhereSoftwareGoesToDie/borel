{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
module Borel.Ceilometer.Cumulative
       ( cumulativeQuery )
where

import           Control.Monad.Logger
import           Control.Monad.Trans
import           Control.Monad.Trans.Reader
import           Data.Word
import           Pipes.Safe

import           Marquise.Client
import           Vaultaire.Control.Lift
import           Vaultaire.Query

import           Borel.Types
import           Borel.Log

cumulativeQuery  :: (MonadSafe m, MonadLogger m, ReaderT BorelEnv `In` m)
                 => Origin -> Address -> TimeStamp -> TimeStamp -> Query m Word64
cumulativeQuery o a s e =
  logInfoThen (concat ["Running cumulative query for address ", show a]) $ do
    env <- liftT ask
    v   <- lift $ aggregateCumulativePoints $ metrics (marquiseReader $ config env) o a s e
    one v
