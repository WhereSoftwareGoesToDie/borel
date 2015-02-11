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
import           Data.ByteString       (ByteString)
import           Data.Text             (Text)
import           Data.Word
import           Pipes
import qualified Pipes.Aeson.Unchecked as PA

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

class ToJSONStream a where
  toJSONStream :: Monad m => Producer a m () -> Producer ByteString m ()

-- copied from old borel
instance ToJSONStream ResponseItem where
  toJSONStream sauce = do
      yield "[\n"
      x <- lift $ next sauce
      case x of Left  _         -> yield "\n]"
                Right (y, rest) -> PA.encode y >> go rest
    where go metric = do
            x <- lift $ next metric
            case x of Left  _         -> yield "\n]"
                      Right (y, rest) -> yield ",\n" >> PA.encode y >> go rest

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
