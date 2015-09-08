-- | Copyright 2014-2015 Anchor Systems, Pty Ltd and Others
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
  , paramBorelConfig
  , paramFlavorMap
  , paramOrigin
  , paramCandideHost, paramCandidePort, paramCandideUser, paramCandidePass

    -- * Query arguments
  , BorelEnv
  , TenancyID(..)
  , GroupedMetric
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
import           Control.Lens            (makeLenses, mapped, over, _2)
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.Bimap              as BM
import qualified Data.Configurator       as C
import qualified Data.Configurator.Types as C
import qualified Data.HashMap.Strict     as HM
import qualified Data.List               as L
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
import           Data.Word
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



type GroupedMetric = [Metric]

--------------------------------------------------------------------------------

newtype TenancyID = TenancyID { _tenancyID :: Text }
  deriving (Eq)

instance Show TenancyID where
  show = show . _tenancyID

instance ToJSON TenancyID where
  toJSON (TenancyID x) = object [ "tenancy-id" .= x ]

instance Read TenancyID where
  readsPrec _ = maybe [] (pure . (,"")) . Just . TenancyID . T.pack

--------------------------------------------------------------------------------

-- | BorelConfigure Borel globals that persist across many queries.
--   (can be reloaded).
--
data BorelConfig = BorelConfig
  { _paramOrigin       :: Origin
  , _paramCandideHost  :: String
  , _paramCandidePort  :: Word16
  , _paramCandideUser  :: String
  , _paramCandidePass  :: String
  , _paramFlavorMap    :: FlavorMap
  , _allInstances      :: Set Metric
  , _allMetrics        :: Set Metric }

makeLenses ''BorelConfig

mkBorelConfig :: Origin -> String -> Word16 -> String -> String -> FlavorMap -> BorelConfig
mkBorelConfig org host port user pass fm
  = BorelConfig org host port user pass fm (S.fromList fs) (S.fromList ms)
  where ms :: [Metric]
        ms = fs <>
          [ diskReads, diskWrites
          , neutronIn, neutronOut
          , cpu, vcpus, memory, ipv4, block, ssd
          , snapshot, image
          ]
        fs :: [Metric]
        fs = map mkInstance (BM.keys fm)

parseBorelConfig :: C.Config -> IO (Either BorelError BorelConfig)
parseBorelConfig raw = do
  flavors <- enumFlavors raw
  liftM6 mkBorelConfig
    <$> (note (ConfigLoad "cannot read origin")          <$> C.lookup raw nameOrigin)
    <*> (note (ConfigLoad "cannot read postgres host")   <$> C.lookup raw nameHost)
    <*> (note (ConfigLoad "cannot read postgres port")   <$> C.lookup raw namePort)
    <*> (note (ConfigLoad "cannot read postgres user")   <$> C.lookup raw nameUser)
    <*> (note (ConfigLoad "cannot read postgres pass")   <$> C.lookup raw namePass)
    <*> (note (ConfigLoad "cannot read flavors")         <$> (fmap mkFM . T.sequenceA <$> mapM lookupFlavor flavors))

  where nameOrigin      = "origin"
        nameHost        = "postgres-host"
        namePort        = "postgres-port"
        nameUser        = "postgres-user"
        namePass        = "postgres-pass"
        nameFlavorGroup = "flavors."

        lookupFlavor :: C.Name -> IO (Maybe (Text, Text))
        lookupFlavor name
          =   fmap (name,)
          <$> C.lookup raw (nameFlavorGroup <> name <> ".id")

        mkFM :: [(Text, Text)] -> FlavorMap
        mkFM = BM.fromList . over (mapped._2) siphashID

        enumFlavors conf = do
          m <- C.getMap conf
          return
            $ L.nub
            $ map (T.takeWhile (/= '.') . fromMaybe "" . T.stripPrefix nameFlavorGroup)
            $ HM.keys
            $ HM.filterWithKey (\k _ -> nameFlavorGroup `T.isPrefixOf` k) m

        -- Do not just lest ye be judged
        liftM6  :: (Monad m) => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m a6 -> m r
        liftM6 f m1 m2 m3 m4 m5 m6 = do { x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; x5 <- m5; x6 <- m6; return (f x1 x2 x3 x4 x5 x6) }


loadBorelConfig :: FilePath -> IO (Either BorelError (BorelConfig, ThreadId))
loadBorelConfig filepath = do
  -- TODO add handler for config loading failure
  (raw, t) <- C.autoReload C.autoConfig [C.Required filepath]
  conf     <- parseBorelConfig raw
  return $ (,t) <$> conf

instance C.Configured Origin where
  convert (C.String s) = hush $ makeOrigin $ T.encodeUtf8 s
  convert _            = Nothing

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
