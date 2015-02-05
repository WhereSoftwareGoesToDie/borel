-- | Copyright 2013-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--
-- /Description/
-- This module defines user-facing types and environment for
-- Borel requests.
--
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Borel.Types
  ( -- * Environment
    Config, BorelEnv, BorelM
  , paramStart, paramEnd, paramFlavorMap, paramMetrics
  , paramOrigin, paramMarquiseURI, paramChevalierURI
    -- * Running
  , runBorel
  , defaultStart, defaultEnd, parseConfig
    -- * Re-exports
  , module Borel.Types.Metric
  , module Borel.Types.UOM
  ) where

import           Control.Applicative
import           Control.Monad.Reader
import           Pipes
import           Control.Lens
import           Data.Set                   (Set)
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Network.URI
import           Pipes.Lift

import           Marquise.Types
import           Vaultaire.Types
import           Ceilometer.Types

import           Borel.Types.Metric
import           Borel.Types.UOM


-- | Configure Borel globals that persist across many queries.
--   (can be reloaded).
--
data Config = Config
  { _origins      :: Set Origin
  , _readerURI    :: URI
  , _chevalierURI :: URI
  , _flavorMap    :: FlavorMap }

makeLenses ''Config

data BorelEnv = BorelEnv
  { _borelConfig  :: Config
  , _paramMetrics :: [Metric]
  , _paramStart   :: TimeStamp
  , _paramEnd     :: TimeStamp
  }

makeLenses ''BorelEnv

paramOrigin :: Lens' BorelEnv (Set Origin)
paramOrigin       = borelConfig . origins

paramMarquiseURI, paramChevalierURI :: Lens' BorelEnv URI
paramMarquiseURI  = borelConfig . readerURI
paramChevalierURI = borelConfig . chevalierURI

paramFlavorMap :: Lens' BorelEnv FlavorMap
paramFlavorMap    = borelConfig . flavorMap

defaultStart :: IO TimeStamp
defaultStart =   getCurrentTimeNanoseconds
             >>= return . addTimeStamp ((-7) * posixDayLength)
  where addTimeStamp x a = convertToTimeStamp
                         $ addUTCTime (convertToDiffTime a) (posixSecondsToUTCTime x)

defaultEnd :: IO TimeStamp
defaultEnd = getCurrentTimeNanoseconds

parseConfig :: FilePath -> IO Config
parseConfig = undefined


newtype BorelM m a = BorelM { borelM :: ReaderT BorelEnv m a }
  deriving ( Functor, Applicative, Monad, MonadTrans
           , MonadReader BorelEnv )

runBorel :: Monad m
         => Config
         -> [Metric]
         -> TimeStamp
         -> TimeStamp
         -> Producer x (BorelM m) ()
         -> Producer x m ()
runBorel conf ms s e p = runReaderP (BorelEnv conf ms s e)
                          $ hoist borelM p
