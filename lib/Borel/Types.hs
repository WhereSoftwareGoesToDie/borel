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
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Borel.Types
  ( -- * Environment
    Config, BorelEnv, BorelM, TenancyID
  , paramStart, paramEnd, paramFlavorMap
  , paramMetrics, paramTID
  , paramOrigin, paramMarquiseURI, paramChevalierURI
    -- * Running
  , runBorel
  , defaultStart, defaultEnd, parseConfig
    -- * Re-exports
  , module Borel.Types.Metric
  , module Borel.Types.UOM
  ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Reader
import           Data.Set              (Set)
import           Data.Text             (Text)
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Network.URI
import           Pipes
import           Pipes.Lift
import           Pipes.Safe

import           Ceilometer.Types
import           Marquise.Types
import           Vaultaire.Types

import           Borel.Types.Metric
import           Borel.Types.UOM


type TenancyID = Text

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
  , _paramTID     :: TenancyID
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
defaultStart =  liftM (addTimeStamp ((-7) * posixDayLength)) getCurrentTimeNanoseconds
  where addTimeStamp x a = convertToTimeStamp
                         $ addUTCTime (convertToDiffTime a) (posixSecondsToUTCTime x)

defaultEnd :: IO TimeStamp
defaultEnd = getCurrentTimeNanoseconds

parseConfig :: FilePath -> IO Config
parseConfig = undefined


newtype BorelM m a = BorelM { borelM :: ReaderT BorelEnv m a }
  deriving ( Functor, Applicative, Monad
           , MonadTrans, MonadIO
           , MonadThrow, MonadMask, MonadCatch
           , MonadReader BorelEnv )

instance MonadSafe m => MonadSafe (BorelM m) where
  type Base (BorelM m) = Base m
  liftBase = lift . liftBase
  register = lift . register
  release  = lift . release

runBorel :: Monad m
         => Config
         -> [Metric]
         -> TenancyID
         -> TimeStamp
         -> TimeStamp
         -> Producer x (BorelM m) ()
         -> Producer x m ()
runBorel conf ms t s e p = runReaderP (BorelEnv conf ms t s e)
                         $ hoist borelM p
