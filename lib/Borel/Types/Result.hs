{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Copyright 2013-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--
-- /Description/
-- This module defines the Result type and how it's presented.
--
module Borel.Types.Result
  ( -- * Query results
    Result
  , ResponseItem(..)
  , respResource, respResourceID
  , respUOM, respQuantity                  
  , mkItem

    -- * Convenient
  , traverseUOMVal
  ) where

import           Control.Monad
import           Control.Applicative
import           Control.Lens        hiding ((.=))
import           Data.Aeson          hiding (Result)
import           Data.Text           (Text)
import           Data.Word

import           Ceilometer.Tags
import           Vaultaire.Types

import           Borel.Error
import           Borel.Types.Metric
import           Borel.Types.UOM

type Result = (Metric, Word64)

data ResponseItem = ResponseItem
  { _respResource   :: Text
  , _respResourceID :: Text
  , _respUOM        :: UOM
  , _respQuantity   :: Word64 }
  deriving (Eq, Show)

makeLenses ''ResponseItem

instance FromJSON ResponseItem where
  parseJSON (Object x)
    =   ResponseItem
    <$> x .: "resource"
    <*> x .: "resource-id"
    <*> x .: "uom"
    <*> x .: "quantity"
  parseJSON _ = mzero

instance ToJSON ResponseItem where
  toJSON (ResponseItem n i u x)
    = object [ "resource"    .= n
             , "resource-id" .= i
             , "uom"         .= u
             , "quantity"    .= x ]

mkItem :: SourceDict -> Result -> ResponseItem
mkItem sd (metric, quantity)
  = let (name, uid) = (keyMetricName, keyResourceID)
                    & both %~ (stopBorelError . flip lookupSD sd)
    in  ResponseItem name uid (uom metric) quantity

-- Some convenient traversals

traverseUOMVal :: Traversal' ResponseItem (UOM, Word64)
traverseUOMVal f (ResponseItem x y u v)
  =   ResponseItem x y
  <$> (fst <$> f (u,v))
  <*> (snd <$> f (u,v))
