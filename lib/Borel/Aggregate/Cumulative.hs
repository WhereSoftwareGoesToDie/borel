{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
module Borel.Aggregate.Cumulative
       ( aggregateCumulative )
where

import           Control.Monad.Logger
import           Control.Monad.Trans
import           Data.Word
import           Pipes.Safe

import           Marquise.Client
import           Vaultaire.Query

import           Borel.Log

aggregateCumulative
  :: (MonadSafe m, MonadLogger m)
  => Query m SimplePoint -> Query m Word64
aggregateCumulative source =
  logInfoThen "Running aggregateCumulative query for address " $ do
    v <- lift $ aggregateCumulativePoints source
    one v
