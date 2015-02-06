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
    Config(..), mkConfig, allInstances, allMetrics
  , BorelEnv
  , paramStart, paramEnd, paramFlavorMap
  , paramMetrics, paramTID
  , paramOrigin, paramMarquiseURI, paramChevalierURI
  , TenancyID
    -- * Running
  , BorelS
  , runBorel
  , defaultStart, defaultEnd
    -- * Re-exports
  , module X
  ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Reader
import qualified Data.Bimap            as BM
import           Data.Monoid
import           Data.Set              (Set)
import qualified Data.Set              as S
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

import           Borel.Types.Error     as X
import           Borel.Types.Metric    as X
import           Borel.Types.Result    as X
import           Borel.Types.UOM       as X


type TenancyID = Text

-- | Configure Borel globals that persist across many queries.
--   (can be reloaded).
--
data Config = Config
  { _origins      :: Set Origin
  , _readerURI    :: URI
  , _chevalierURI :: URI
  , _flavorMap    :: FlavorMap
  , _instances    :: Set Metric
  , _metrics      :: Set Metric }

makeLenses ''Config

mkConfig :: Set Origin -> URI -> URI -> FlavorMap -> Config
mkConfig org marq chev fm
  = Config org marq chev fm (S.fromList fs) (S.fromList ms)
  where ms :: [Metric]
        ms = fs <>
          [ diskReads, diskWrites
          , neutronIn, neutronOut
          , cpu, vcpus, memory, ipv4, volumes
          , snapshot, image
          ]
        fs :: [Metric]
        fs = map mkInstance (BM.keys fm)


data BorelEnv = BorelEnv
  { _borelConfig  :: Config
  , _paramMetrics :: Set Metric
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
paramFlavorMap = borelConfig . flavorMap

allInstances :: Lens' BorelEnv (Set Metric)
allInstances = borelConfig . instances

allMetrics :: Lens' BorelEnv (Set Metric)
allMetrics = borelConfig . metrics

defaultStart :: IO TimeStamp
defaultStart =  liftM (addTimeStamp ((-7) * posixDayLength)) getCurrentTimeNanoseconds
  where addTimeStamp x a = convertToTimeStamp
                         $ addUTCTime (convertToDiffTime a) (posixSecondsToUTCTime x)

defaultEnd :: IO TimeStamp
defaultEnd = getCurrentTimeNanoseconds


newtype BorelS m a = BorelS { borelM :: ReaderT BorelEnv m a }
  deriving ( Functor, Applicative, Monad
           , MonadTrans, MonadIO
           , MonadThrow, MonadMask, MonadCatch
           , MonadReader BorelEnv )

instance MonadSafe m => MonadSafe (BorelS m) where
  type Base (BorelS m) = Base m
  liftBase = lift . liftBase
  register = lift . register
  release  = lift . release

runBorel :: Monad m
         => Config
         -> Set Metric
         -> TenancyID
         -> TimeStamp
         -> TimeStamp
         -> Producer x (BorelS m) ()
         -> Producer x m ()
runBorel conf ms t s e p = runReaderP (BorelEnv conf ms t s e)
                         $ hoist borelM p
