{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Copyright 2014-2015 Anchor Systems, Pty Ltd and Others
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
  , respResource, respResourceID, respVal
  , mkItem

    -- * Convenient
  , uomVal
  ) where

import           Control.Applicative
import           Control.Lens        hiding ((.=))
import           Control.Monad
import           Data.Aeson          hiding (Result)
import           Data.Csv            (FromRecord, ToRecord, parseRecord, record,
                                      toField, toRecord, (.!))
import           Data.Text           (Text)
import qualified Data.Vector         as V
import           Data.Word

import           Ceilometer.Tags
import           Vaultaire.Types

import           Borel.Error
import           Borel.Types.Metric
import           Borel.Types.UOM

type Result = (Metric, Word64)
type Val    = (UOM, Word64)

data ResponseItem = ResponseItem
  { _respResource   :: Text
  , _respResourceID :: Text
  , _respVal        :: Val }
  deriving (Eq, Show, Read)

makeLenses ''ResponseItem

instance FromJSON ResponseItem where
  parseJSON (Object x)
    =   ResponseItem
    <$>          x .: "resource"
    <*>          x .: "resource-id"
    <*> ((,) <$> x .: "uom"
             <*> x .: "quantity")
  parseJSON _ = mzero

instance ToJSON ResponseItem where
  toJSON (ResponseItem n i (u,x))
    = object [ "resource"    .= n
             , "resource-id" .= i
             , "uom"         .= u
             , "quantity"    .= x ]

instance FromRecord ResponseItem where
  parseRecord v
    | V.length v == 4 = ResponseItem
                     <$>          v .! 0
                     <*>          v .! 1
                     <*> ((,) <$> v .! 2
                              <*> v .! 3)

    | otherwise = mzero

instance ToRecord ResponseItem where
  toRecord (ResponseItem n i (u,v))
    = record [ toField n
             , toField i
             , toField u
             , toField v ]

mkItem :: SourceDict -> Result -> ResponseItem
mkItem sd (metric, quantity)
  = let name = pretty metric
        uid  = stopBorelError $ lookupSD keyResourceID sd
    in  ResponseItem name uid (uom metric, quantity)

-- Some convenient traversals

uomVal :: Lens' ResponseItem Val
uomVal f (ResponseItem x y v)
  =   ResponseItem x y <$> f v
