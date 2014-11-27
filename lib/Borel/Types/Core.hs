{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- * Borel request/response and environment types.
--
module Borel.Types.Core
  ( -- * Metrics
    Format(..)
  , Quantity
  , Customer(..)
  , Metric(..)
  , mkMetric, streamSummary
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

import           Borel.Types.Resource


data Format = CSV | JSON | Summary

type Quantity = Word64

newtype Customer = Customer { cid :: Int }
  deriving (Parsable, Eq, Ord, Read)

data Metric = Metric
    { mCustomer   :: Customer
    , mResource   :: Resource
    , mUOM        :: UOM
    , mQuantity   :: Quantity
    , mIdentifier :: (Text, Maybe Text)
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
