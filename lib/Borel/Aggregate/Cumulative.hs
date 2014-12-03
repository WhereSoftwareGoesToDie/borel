{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
module Borel.Aggregate.Cumulative
       ( aggregateCumulative )
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

aggregateCumulative  :: (MonadSafe m, MonadLogger m, ReaderT BorelEnv `In` m)
                 => Origin -> Address -> TimeStamp -> TimeStamp -> Query m Word64
aggregateCumulative o a s e =
  logInfoThen (concat ["Running aggregateCumulative query for address ", show a]) $ do
    env <- liftT ask
    v   <- lift $ aggregateCumulativePoints
        $ metrics (_readerURI $ config env) o a s e
    one v
