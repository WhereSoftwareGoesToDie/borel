{-# LANGUAGE OverloadedStrings #-}
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
  ( Result
  , ResponseItem(..)
  , mkItem
  ) where

import           Control.Lens          hiding ((.=))
import           Data.Aeson            hiding (Result)
import           Data.Text             (Text)
import           Data.Word

import           Ceilometer.Tags
import           Vaultaire.Types

import           Borel.Error
import           Borel.Types.Metric
import           Borel.Types.UOM

type Result = (Metric, Word64)

data ResponseItem = ResponseItem
  { _resource         :: Text
  , _resourceID       :: Text
  , _resourceUOM      :: UOM
  , _resourceQuantity :: Word64 }

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
