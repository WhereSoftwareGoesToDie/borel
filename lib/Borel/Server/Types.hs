{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ExistentialQuantification #-}

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
         -- * Domain of resources, e.g. OpenStack and SomethingElse (tm)
       , Domain(..), Source(..)
       )
where

import           Control.Monad.Trans.Reader
import           Data.Text                  (Text)
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Network.URI
import           Pipes
import           Pipes.Lift

import           Marquise.Types
import           Vaultaire.Types
import           Borel.Types.Resource (ResourceGroup)

data Domain = forall a. Source a => Domain a

-- | Parameters for one Borel query.
data BorelEnv = BorelEnv
  { config :: BackendConfig
  , start  :: TimeStamp
  , end    :: TimeStamp }

-- | Interface to deal with different source of resources,
--   e.g. OpenStack and SomethingElse (tm)
--   indexed by the type of resource sources
--
class Source sauce where
  originsOf :: sauce -> [Origin]      -- ^ Resource providers to origins which track that provider
  sourceKey :: sauce -> Text          -- ^ Resource provider key in Vaultaire metadata
  domain    :: ResourceGroup -> sauce -- ^ Each logical group of resources should be managed by one domain

-- | Configure the Vaultaire-related backends that Borel uses,
--   e.g. Marquise, Chevalier.
--
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
