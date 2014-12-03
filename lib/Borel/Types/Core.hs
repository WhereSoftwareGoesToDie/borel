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
    Format(..)
  , Quantity
  , Customer(..)
  , Result(..)
  , ResourceID
  , mkResult, streamSummary
    -- * Domain of resources, e.g. OpenStack and SomethingElse (tm)
  , Domain(..)
  , domainOrigins, domainIdentKey, domainTagKey
  , Providing(..)
  ) where

import           Data.Aeson.TH
import qualified Data.ByteString.Char8 as B
import           Data.Map              (Map)
import qualified Data.Map              as M
import           Data.Text             (Text)
import           Data.Word
import           Pipes
import qualified Pipes.Prelude         as P
import           Web.Scotty            (Parsable)

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
  originsOf   :: sauce -> [Origin]       -- ^ Resource providers to origins which track that provider
  resourceKey :: sauce -> Text           -- ^ Resource descriptor key
  metricKey   :: sauce -> Text           -- ^ Metric descriptor key
  domain      :: MetricGroup -> sauce    -- ^ Each logical group of resources should be managed by one domain

-- boilerplate

domainOrigins     (Domain a) = originsOf a
domainIdentKey    (Domain a) = resourceKey a
domainTagKey      (Domain a) = metricKey a


-- Intermediate types ----------------------------------------------------------

data Format = CSV | JSON | Summary

type Quantity = Word64

newtype Customer = Customer { cid :: Int }
  deriving (Parsable, Eq, Ord, Read)

type ResourceID = ( Text   -- ^ sourcedict key
                  , Text ) -- ^ sourcedict value that uniquely descibes the resource

data Result = Result
    { resultCustomer   :: Customer
    , resultMetric     :: Metric
    , resultUOM        :: UOM
    , resultQuantity   :: Quantity
    , resultIdentifier :: ResourceID
    } deriving Show

mkResult :: Customer -> Metric -> Quantity -> (Text, Text) -> Result
mkResult c r = Result c r (uom r)


-- (De-)Serialisation ----------------------------------------------------------

instance Show Customer where
    show (Customer x) = show x

$(deriveJSON defaultOptions ''Format)

summarise :: Monad m => Producer Result m () -> m (Map (Customer, Metric, UOM) Quantity)
summarise = P.fold (\acc (Result c r u q _) -> M.insertWith (+) (c, r, u) q acc) M.empty id

streamSummary :: Monad m => Producer Result m () -> Producer B.ByteString m ()
streamSummary p = do
    summary <- lift $ summarise p
    mapM_ (\((c, r, u), q) -> yield $ format c r u q) $ M.assocs summary
  where format c r u q = B.pack $ concat
                       [ show $ cid c, ", "
                       , pretty r, ", "
                       , show u, ", "
                       , show q, "\n"]
