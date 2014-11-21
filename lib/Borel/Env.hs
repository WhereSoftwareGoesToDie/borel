{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TypeOperators     #-}

module Borel.Env
       ( -- * Environment
         BorelEnv(..), BackendConfig(..)
       , backendConf, defaultStart, defaultEnd
       , runBorel
       , serialise
       )
where

import           Control.Monad.Trans.Reader
import           Data.Default
import           Data.Maybe
import           Data.Text                  (Text)
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Network.URI
import           Pipes
import           Pipes.Lift

import           Borel.Types
import           Marquise.Types
import           Vaultaire.Types


-- | Parameters for one Borel query.
data BorelEnv = BorelEnv
  { config :: BackendConfig
  , start  :: TimeStamp
  , end    :: TimeStamp }

-- | Configure the Vaultaire-related backends that Borel uses,
--   e.g. Marquise, Chevalier.
--
data BackendConfig = BackendConfig
  { origins         :: [Origin]
  , originsOfDomain :: Domain -> [Origin]
  , domainIdentKey  :: Domain -> Text --The key in a sourcedict to pull the identifier
  , marquiseReader  :: URI
  , chev            :: URI
  }

instance Default BackendConfig where
  def = BackendConfig orgs originMapping identKeyMapping r c
    where orgs = [ceilometer]
          originMapping d = case d of
            OpenStack -> [ceilometer]
          ceilometer      = read "ABCDEF"
          identKeyMapping OpenStack = "resource_id"
          c = fromJust $ parseURI "tcp://chevalier.example.com:6283"
          r = fromJust $ parseURI "tcp://broker.example.com:5570"

backendConf :: URI -> URI -> BackendConfig
backendConf x y = def { marquiseReader = x, chev = y }

defaultStart :: IO TimeStamp
defaultStart = getCurrentTimeNanoseconds >>= return . addTimeStamp ((-7) * posixDayLength)

defaultEnd :: IO TimeStamp
defaultEnd = getCurrentTimeNanoseconds

addTimeStamp :: POSIXTime -> TimeStamp -> TimeStamp
addTimeStamp x a = convertToTimeStamp $ addUTCTime (convertToDiffTime a) (posixSecondsToUTCTime x)

runBorel :: Monad m
          => BackendConfig
          -> TimeStamp
          -> TimeStamp
          -> Producer x (ReaderT BorelEnv m) ()
          -> Producer x m ()
runBorel conf s e = runReaderP (BorelEnv conf s e)
