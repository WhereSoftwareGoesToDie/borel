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
module Borel.Types.Env
       ( -- * Environment
         BorelEnv
       , paramStart, paramEnd, paramOrigin, paramMarquiseURI, paramChevalierURI, paramFlavorMap
       , defaultStart, defaultEnd
       , runBorel
       )
where

import           Control.Lens
import           Control.Monad.Trans.Reader
import           Data.Set                   (Set)
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Network.URI
import           Pipes
import           Pipes.Lift

import           Marquise.Types
import           Vaultaire.Types
import           Ceilometer.Types


-- | Parameters for one Borel query.
type Params = (TimeStamp, TimeStamp)

-- | Configure Borel globals that persist across many queries.
--   (can be reloaded).
--
data Config = Config
  { _origins      :: Set Origin
  , _readerURI    :: URI
  , _chevalierURI :: URI
  , _flavorMap    :: FlavorMap }

makeLenses ''Config

type BorelEnv = (Params, Config)

paramStart, paramEnd :: Lens' BorelEnv TimeStamp
paramStart = _1 . _1
paramEnd   = _1 . _2

paramOrigin :: Lens' BorelEnv (Set Origin)
paramOrigin       = _2 . origins

paramMarquiseURI, paramChevalierURI :: Lens' BorelEnv URI
paramMarquiseURI  = _2 . readerURI
paramChevalierURI = _2 . chevalierURI

paramFlavorMap :: Lens' BorelEnv FlavorMap
paramFlavorMap    = _2 . flavorMap

defaultStart :: IO TimeStamp
defaultStart =   getCurrentTimeNanoseconds
             >>= return . addTimeStamp ((-7) * posixDayLength)
  where addTimeStamp x a = convertToTimeStamp
                         $ addUTCTime (convertToDiffTime a) (posixSecondsToUTCTime x)

defaultEnd :: IO TimeStamp
defaultEnd = getCurrentTimeNanoseconds

runBorel :: Monad m
         => Config
         -> TimeStamp
         -> TimeStamp
         -> Producer x (ReaderT BorelEnv m) ()
         -> Producer x m ()
runBorel conf s e = runReaderP ((s,e), conf)
