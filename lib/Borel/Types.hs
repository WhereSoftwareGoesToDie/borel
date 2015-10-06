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
  , paramWorkers
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
import           Control.Monad.Error.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
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
  { _paramWorkers     :: Int
  , _paramOrigin      :: Origin
  , _paramCandideHost :: String
  , _paramCandidePort :: Word16
  , _paramCandideUser :: String
  , _paramCandidePass :: String
  , _paramFlavorMap   :: FlavorMap
  , _allInstances     :: Set Metric
  , _allMetrics       :: Set Metric }

makeLenses ''BorelConfig

mkBorelConfig :: Int -> Origin -> String -> Word16 -> String -> String -> FlavorMap -> BorelConfig
mkBorelConfig workers org host port user pass fm
  = BorelConfig workers org host port user pass fm (S.fromList fs) (S.fromList ms)
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
parseBorelConfig raw = runExceptT $ do
  flavors <- enumFlavors raw
  mkBorelConfig
    <$> lookup' "cannot read number of workers" nameWorkers
    <*> lookup' "cannot read origin"            nameOrigin
    <*> lookup' "cannot read postgres host"     nameHost
    <*> lookup' "cannot read postgres port"     namePort
    <*> lookup' "cannot read postgres user"     nameUser
    <*> lookup' "cannot read postgres pass"     namePass
    <*> (mkFM <$> mapM lookupFlavor flavors)

  where nameWorkers     = "num-workers"
        nameOrigin      = "origin"
        nameHost        = "postgres-host"
        namePort        = "postgres-port"
        nameUser        = "postgres-user"
        namePass        = "postgres-pass"
        nameFlavorGroup = "flavors."

        lookup' :: (C.Configured a) => Text -> C.Name -> ExceptT BorelError IO a
        lookup' err name = do
          res <- liftIO $ C.lookup raw name
          case res of
            Just x  -> return x
            Nothing -> throwError (ConfigLoad err)

        lookupFlavor :: C.Name -> ExceptT BorelError IO (Text, Text)
        lookupFlavor name = lookup' "cannot read flavors" (nameFlavorGroup <> name <> ".id")

        mkFM :: [(Text, Text)] -> FlavorMap
        mkFM = BM.fromList . over (mapped._2) siphashID

        enumFlavors conf = do
          m <- liftIO $ C.getMap conf
          return
            $ L.nub
            $ map (T.takeWhile (/= '.') . fromMaybe "" . T.stripPrefix nameFlavorGroup)
            $ HM.keys
            $ HM.filterWithKey (\k _ -> nameFlavorGroup `T.isPrefixOf` k) m


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
