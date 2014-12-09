{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
module Borel.Aggregate.Delta
       ( aggregateDelta )
where

import           Control.Monad.Logger
import           Data.Word
import           Pipes.Safe

import           Marquise.Client
import           Vaultaire.Query

import           Borel.Log

aggregateDelta :: (MonadSafe m, MonadLogger m)
               => Query m SimplePoint
               -> Query m Word64
aggregateDelta source = logInfoThen "Running gauge query" $ sumPoints source
