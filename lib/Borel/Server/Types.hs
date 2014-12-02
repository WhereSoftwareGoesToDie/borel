{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TypeOperators     #-}

-- | Types required to run a Borel server and query, this includes
--
--   * Backends: Marquise, Chevalier, which pulls data from Vaultaire
--   * Frontend: query arguments for a query
--
module Borel.Server.Types
       ( -- * Environment
         BorelEnv(..), BackendConfig(..)
       , defaultStart, defaultEnd
       , runBorel
       )
where

import           Control.Monad.Trans.Reader
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Network.URI
import           Pipes
import           Pipes.Lift

import           Marquise.Types
import           Vaultaire.Types
import           Borel.Types.Core     (Domain)


-- | Parameters for one Borel query.
data BorelEnv = BorelEnv
  { config :: BackendConfig
  , start  :: TimeStamp
  , end    :: TimeStamp }

-- | Configure the Vaultaire-related backends that Borel uses,
--   e.g. Marquise, Chevalier.
data BackendConfig = BackendConfig
  { _origins         :: [Origin]
  , _readerURI       :: URI
  , _chevalierURI    :: URI
  , _domain          :: Domain }

defaultStart :: IO TimeStamp
defaultStart = getCurrentTimeNanoseconds >>= return . addTimeStamp ((-7) * posixDayLength)
  where addTimeStamp x a = convertToTimeStamp
                         $ addUTCTime (convertToDiffTime a) (posixSecondsToUTCTime x)

defaultEnd :: IO TimeStamp
defaultEnd = getCurrentTimeNanoseconds

runBorel :: Monad m
          => BackendConfig
          -> TimeStamp
          -> TimeStamp
          -> Producer x (ReaderT BorelEnv m) ()
          -> Producer x m ()
runBorel conf s e = runReaderP (BorelEnv conf s e)
