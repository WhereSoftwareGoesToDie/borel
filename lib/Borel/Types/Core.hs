{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ExistentialQuantification #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- | Borel request/response and domain types
--
module Borel.Types.Core
  ( -- * Metrics
    ResourceID
    -- * Domain of resources, e.g. OpenStack and SomethingElse (tm)
  , Domain(..)
  , domainOrigins, domainIdentKey, domainTagKey, domainIdentKeyPretty
  , Providing(..)
  ) where

import           Data.Text             (Text)

import           Vaultaire.Types
import           Borel.Types.Metric


-- Resource provider interface -------------------------------------------------

-- | Provider of resources, e.g. OpenStack and SomethingElse (tm)
--   resources from different providers can be treated differently
--   in Borel queries.
--
data Domain = forall a. Providing a => Domain a

-- | Interface to deal with different source of resources,
--   e.g. OpenStack and SomethingElse (tm)
--   indexed by the type of resource sources
class Providing sauce where
  originsOf         :: sauce -> [Origin]       -- ^ Resource providers to origins which track that provider
  resourceKey       :: sauce -> Text           -- ^ Resource descriptor key
  resourceKeyPretty :: sauce -> Text           -- ^ Resource descriptor key used for display
  metricKey         :: sauce -> Text           -- ^ Metric descriptor key
  domain            :: MetricGroup -> sauce    -- ^ Each logical group of resources should be managed by one domain

-- boilerplate

domainOrigins        (Domain a) = originsOf a
domainIdentKey       (Domain a) = resourceKey a
domainIdentKeyPretty (Domain a) = resourceKeyPretty a
domainTagKey         (Domain a) = metricKey a


-- Intermediate types ----------------------------------------------------------

type ResourceID = ( Text   -- ^ sourcedict key
                  , Text ) -- ^ sourcedict value that uniquely descibes the resource
