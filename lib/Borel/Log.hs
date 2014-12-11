{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Borel.Log
  ( runLogger
  , runLoggerP
  , logInfoThen
  , logDebugStr
  , logInfoStr
  , logErrorStr
  , logWarnStr
  , logOtherStr
  ) where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.ByteString.Char8  as BC (hPutStrLn)
import           Data.Monoid
import qualified Data.Text              as T
import           Pipes
import           Pipes.Lift
import           Pipes.Safe
import           System.IO
import           System.Log.FastLogger

import           Vaultaire.Types

newtype ResourceLogger m a = ResourceLogger (ReaderT LogLevel m a)
    deriving ( Functor, Applicative, Monad
             , MonadReader LogLevel
             , MonadTrans, MFunctor)

runLogger :: (Monad m)
          => LogLevel
          -> ResourceLogger m a
          -> m a
runLogger logLevel (ResourceLogger act) = runReaderT act logLevel

runLoggerP :: (Monad m)
           => LogLevel
           -> Producer x (ResourceLogger m) ()
           -> Producer x m ()
runLoggerP logLevel p = runLogger logLevel $ distribute p

instance (MonadIO m) => MonadIO (ResourceLogger m) where
    liftIO = lift . liftIO

-- This only allows logging within a pipe, but not piped values. See below.
instance (MonadLogger m) => MonadLogger (Proxy a' a b' b m) where
    monadLoggerLog a b c d = lift $ monadLoggerLog a b c d

instance (MonadIO m) => MonadLogger (ResourceLogger m) where
    monadLoggerLog _ _ level msg = do
        logLevel <- ask
        when (level >= logLevel) $ liftIO $ do
            currTime <- getCurrentTimeNanoseconds
            let logPrefix = mconcat $ map toLogStr [showLevel level, " ",  show currTime, " "]
            let output = fromLogStr $ logPrefix <> toLogStr msg
            BC.hPutStrLn stdout output
            when (level == LevelError) $ BC.hPutStrLn stderr output
      where
        showLevel LevelDebug     = "[Debug]"
        showLevel LevelInfo      = "[Info]"
        showLevel LevelWarn      = "[Warning]"
        showLevel LevelError     = "[Error]"
        showLevel (LevelOther l) = concat ["[", show l, "]"]

deriving instance MonadCatch m => MonadCatch (ResourceLogger m)
deriving instance MonadThrow m => MonadThrow (ResourceLogger m)
deriving instance MonadMask  m => MonadMask  (ResourceLogger m)

instance MonadSafe m => MonadSafe (ResourceLogger m) where
  type Base (ResourceLogger m) = Base m
  liftBase = lift . liftBase
  register = lift . register
  release  = lift . release

logInfoThen :: (MonadTrans t, Monad (t m), MonadLogger m) => String -> t m x -> t m x
logInfoThen x a = lift (logInfoStr x) >> a

-- | String versions of log functions

logDebugStr   :: MonadLogger m => String -> m ()
logDebugStr   = logDebugN   . T.pack

logInfoStr    :: MonadLogger m => String -> m ()
logInfoStr    = logInfoN    . T.pack

logWarnStr    :: MonadLogger m => String -> m ()
logWarnStr    = logWarnN    . T.pack

logErrorStr   :: MonadLogger m => String -> m ()
logErrorStr   = logErrorN   . T.pack

logOtherStr   :: MonadLogger m => LogLevel -> String -> m ()
logOtherStr l = logOtherN l . T.pack
