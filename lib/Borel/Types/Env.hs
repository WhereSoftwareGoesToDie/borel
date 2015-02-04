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
         Params, Config
       , parseConfig
       , paramStart, paramEnd, paramOrigin, paramMarquiseURI, paramChevalierURI, paramFlavorMap
       , defaultStart, defaultEnd
       )
where

import           Control.Lens
import           Data.Set                   (Set)
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Network.URI

import           Marquise.Types
import           Vaultaire.Types
import           Ceilometer.Types
