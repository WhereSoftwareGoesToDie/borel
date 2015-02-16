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
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

-- for configuration
{-# OPTIONS -fno-warn-orphans #-}

module Borel.Types
  ( -- * BorelConfig
    BorelConfig(..)
  , mkBorelConfig, parseBorelConfig, loadBorelConfig
  , allInstances, allMetrics
  , paramBorelConfig, paramFlavorMap, paramOrigins, paramMarquiseURI, paramChevalierURI

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
import           Control.Concurrent      (ThreadId)
import           Control.Error.Util
import           Control.Lens            (makeLenses)
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.Bimap              as BM
import qualified Data.Configurator       as C
import qualified Data.Configurator.Types as C
import           Data.Maybe
import           Data.Monoid
import           Data.Set                (Set)
import qualified Data.Set                as S
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import qualified Data.Traversable        as T
import           Data.Word               (Word32)
import           Network.URI
import           Pipes
import           Pipes.Lift
import           Pipes.Safe

import           Ceilometer.Types
import           Marquise.Types
import           Vaultaire.Types

import           Borel.Error             as X
import           Borel.Types.Metric      as X
import           Borel.Types.Result      as X
import           Borel.Types.UOM         as X


--------------------------------------------------------------------------------

newtype TenancyID = TenancyID { _tenancyID :: Text }
  deriving (Eq, Show)

instance ToJSON TenancyID where
  toJSON (TenancyID x) = object [ "tenancy-id" .= x ]


--------------------------------------------------------------------------------

-- | BorelConfigure Borel globals that persist across many queries.
--   (can be reloaded).
--
data BorelConfig = BorelConfig
  { _paramOrigins      :: Set Origin
  , _paramMarquiseURI  :: URI
  , _paramChevalierURI :: URI
  , _paramFlavorMap    :: FlavorMap
  , _allInstances      :: Set Metric
  , _allMetrics        :: Set Metric }

makeLenses ''BorelConfig

mkBorelConfig :: Set Origin -> URI -> URI -> FlavorMap -> BorelConfig
mkBorelConfig org marq chev fm
  = BorelConfig org marq chev fm (S.fromList fs) (S.fromList ms)
  where ms :: [Metric]
        ms = fs <>
          [ diskReads, diskWrites
          , neutronIn, neutronOut
          , cpu, vcpus, memory, ipv4, volumes
          , snapshot, image
          ]
        fs :: [Metric]
        fs = map mkInstance (BM.keys fm)

parseBorelConfig :: C.Config -> IO (Either BorelError BorelConfig)
parseBorelConfig raw = do
  names   <- fromMaybe [] <$> C.lookup raw nameFlavors
  flavors <- mapM lookupFlavor names

  liftM4 mkBorelConfig
    <$> (note (ConfigLoad "cannot read list of origins") <$> C.lookup raw nameOrigins)
    <*> (note (ConfigLoad "cannot read Marquise URI")    <$> C.lookup raw nameMarquise)
    <*> (note (ConfigLoad "cannot read Chevalier URI")   <$> C.lookup raw nameChevalier)
    <*> (note (ConfigLoad "cannot read flavors")         <$> pure (BM.fromList <$> T.sequenceA flavors))

  where nameOrigins     = "origins"
        nameMarquise    = "marquise-reader-uri"
        nameChevalier   = "chevalier-uri"
        nameFlavors     = "instance-flavors"
        nameFlavorGroup = "flavors"

        lookupFlavor :: C.Name -> IO (Maybe (Text, Word32))
        lookupFlavor name
          =   liftA2 (,)
          <$> C.lookup raw (nameFlavorGroup <> "." <> name <> ".id")
          <*> C.lookup raw (nameFlavorGroup <> "." <> name <> ".hash")


loadBorelConfig :: FilePath -> IO (Either BorelError (BorelConfig, ThreadId))
loadBorelConfig filepath = do
  -- TODO add handler for config loading failure
  (raw, t) <- C.autoReload C.autoConfig [C.Required filepath]
  conf     <- parseBorelConfig raw
  return $ (,t) <$> conf

instance C.Configured Origin where
  convert (C.String s) = hush $ makeOrigin $ T.encodeUtf8 s

instance C.Configured (Set Origin) where
  convert (C.List xs) = S.fromList <$> T.mapM C.convert xs
  convert  _          = Nothing

instance C.Configured URI where
  convert (C.String s) = parseURI (T.unpack s)
  convert  _           = Nothing


--------------------------------------------------------------------------------

-- | Parameters for a Borel request
--
data BorelEnv = BorelEnv
  { _paramBorelConfig :: BorelConfig
  , _paramMetrics     :: Set Metric
  , _paramTID         :: TenancyID
  , _paramStart       :: TimeStamp
  , _paramEnd         :: TimeStamp
  }

makeLenses ''BorelEnv

defaultStart :: IO TimeStamp
defaultStart =  liftM (addTimeStamp ((-7) * posixDayLength)) getCurrentTimeNanoseconds
  where addTimeStamp x a = convertToTimeStamp
                         $ addUTCTime (convertToDiffTime a) (posixSecondsToUTCTime x)

defaultEnd :: IO TimeStamp
defaultEnd = getCurrentTimeNanoseconds


--------------------------------------------------------------------------------

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
         => BorelConfig
         -> Set Metric
         -> TenancyID
         -> TimeStamp
         -> TimeStamp
         -> Producer x (BorelS m) ()
         -> Producer x m ()
runBorel conf ms t s e p = runReaderP (BorelEnv conf ms t s e)
                         $ hoist borelM p
