{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TemplateHaskell   #-}
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
         -- * Environment lenses
       , config, queryStart, queryEnd
       , origins, readerURI, chevalierURI
       )
where

import           Control.Lens
import           Control.Monad.Trans.Reader
import           Data.Map                   (Map)
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Data.Word
import           Network.URI
import           Pipes
import           Pipes.Lift

import           Marquise.Types
import           Vaultaire.Types


-- | Parameters for one Borel query.
data BorelEnv = BorelEnv
  { _config     :: BackendConfig
  , _flavorMap  :: Map Word32 String
  , _queryStart :: TimeStamp
  , _queryEnd   :: TimeStamp }

-- | Configure the Vaultaire-related backends that Borel uses,
--   e.g. Marquise, Chevalier.
data BackendConfig = BackendConfig
  { _origins      :: [Origin]
  , _readerURI    :: URI
  , _chevalierURI :: URI }

$(makeLenses ''BorelEnv)
$(makeLenses ''BackendConfig)


defaultStart :: IO TimeStamp
defaultStart = getCurrentTimeNanoseconds >>= return . addTimeStamp ((-7) * posixDayLength)
  where addTimeStamp x a = convertToTimeStamp
                         $ addUTCTime (convertToDiffTime a) (posixSecondsToUTCTime x)

defaultEnd :: IO TimeStamp
defaultEnd = getCurrentTimeNanoseconds

runBorel :: Monad m
         => BackendConfig
         -> Map Word32 String
         -> TimeStamp
         -> TimeStamp
         -> Producer x (ReaderT BorelEnv m) ()
         -> Producer x m ()
runBorel conf flavorMap s e = runReaderP (BorelEnv conf flavorMap s e)
