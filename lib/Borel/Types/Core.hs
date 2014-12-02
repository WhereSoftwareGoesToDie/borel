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
  , Metric(..)
  , MetricIdentifier
  , mkMetric, streamSummary
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
import           Borel.Types.Resource


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
  originsOf   :: sauce -> [Origin]         -- ^ Resource providers to origins which track that provider
  identKey    :: sauce -> Text             -- ^ FIXME: Mystery
  domainKey   :: sauce -> Text             -- ^ FIXME: Mystery two
  domain      :: ResourceGroup -> sauce    -- ^ Each logical group of resources should be managed by one domain

-- boilerplate

domainOrigins     (Domain a) = originsOf a
domainIdentKey    (Domain a) = identKey a
domainTagKey      (Domain a) = domainKey a


-- Intermediate types ----------------------------------------------------------

data Format = CSV | JSON | Summary

type Quantity = Word64

newtype Customer = Customer { cid :: Int }
  deriving (Parsable, Eq, Ord, Read)

type MetricIdentifier = ( Text        -- the sourcedict key, I think, just importing
                        , Maybe Text) -- value

data Metric = Metric
    { mCustomer   :: Customer
    , mResource   :: Resource
    , mUOM        :: UOM
    , mQuantity   :: Quantity
    , mIdentifier :: MetricIdentifier
    } deriving Show

mkMetric :: Customer -> Resource -> Quantity -> (Text, Maybe Text) -> Metric
mkMetric c r = Metric c r (uom r)


-- (De-)Serialisation ----------------------------------------------------------

instance Show Customer where
    show (Customer x) = show x

$(deriveJSON defaultOptions ''Format)

summarise :: Monad m => Producer Metric m () -> m (Map (Customer, Resource, UOM) Quantity)
summarise = P.fold (\acc (Metric c r u q _) -> M.insertWith (+) (c, r, u) q acc) M.empty id

streamSummary :: Monad m => Producer Metric m () -> Producer B.ByteString m ()
streamSummary p = do
    summary <- lift $ summarise p
    mapM_ (\((c, r, u), q) -> yield $ format c r u q) $ M.assocs summary
  where format c r u q = B.pack $ concat
                       [ show $ cid c, ", "
                       , pretty r, ", "
                       , show u, ", "
                       , show q, "\n"]
