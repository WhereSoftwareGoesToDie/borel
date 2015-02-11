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
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Borel.Types
  ( -- * Config
    Config(..), mkConfig
  , allInstances, allMetrics
  , paramConfig, paramFlavorMap, paramOrigins, paramMarquiseURI, paramChevalierURI

    -- * Query arguments
  , BorelEnv
  , TenancyID(..)
  , paramStart, paramEnd, paramMetrics, paramTID

    -- * Running
  , BorelS
  , runBorel
  , defaultStart, defaultEnd

    -- * Re-exports
  , module X
  ) where

import           Control.Applicative
import           Control.Lens          (makeLenses)
import           Control.Monad.Reader
import           Data.Aeson
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

import           Borel.Types.Metric    as X
import           Borel.Types.Result    as X
import           Borel.Types.UOM       as X


newtype TenancyID = TenancyID { _tenancyID :: Text }
  deriving (Eq, Show)

instance ToJSON TenancyID where
  toJSON (TenancyID x) = object [ "tenancy-id" .= x ]

-- | Configure Borel globals that persist across many queries.
--   (can be reloaded).
--
data Config = Config
  { _paramOrigins      :: Set Origin
  , _paramMarquiseURI  :: URI
  , _paramChevalierURI :: URI
  , _paramFlavorMap    :: FlavorMap
  , _allInstances      :: Set Metric
  , _allMetrics        :: Set Metric }

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
  { _paramConfig  :: Config
  , _paramMetrics :: Set Metric
  , _paramTID     :: TenancyID
  , _paramStart   :: TimeStamp
  , _paramEnd     :: TimeStamp
  }

makeLenses ''BorelEnv

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
