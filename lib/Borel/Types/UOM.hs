-- | Copyright 2013-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--
-- /Description/
-- This module defines unit of measurement for metric reports.
--

{-# OPTIONS -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

module Borel.Types.UOM
  ( -- * Unit of measurement
    UOM(..), BaseUOM(..), Prefix(..)

    -- * Convenient constructors
  , nanoSec, byte, megabyte, gigabyte

    -- * Utilities
  , convert
  , pUOM, pPrefixUOM, pBaseUOM
  ) where

import           Control.Applicative
import           Control.Error.Util
import           Control.Lens         (Prism', preview, prism', re, review,
                                       (^.), (^?))
import           Control.Monad
import           Data.Aeson
import qualified Data.Attoparsec.Text as AT
import           Data.Monoid
import           Data.MultiSet        (MultiSet)
import qualified Data.MultiSet        as S
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Word


nanoSec  = UOM Nano Second
byte     = UOM Base Byte
megabyte = UOM Mega Byte
gigabyte = UOM Giga Byte

data UOM
  = UOM Prefix BaseUOM
  | Times UOM UOM
  deriving (Eq, Ord)

data Prefix
  = Base
  | Giga
  | Nano
  | Mebi
  | Mega
  deriving (Eq, Ord)

data BaseUOM
  = Second
  | Hour
  | Byte
  | Instance
  | IPAddress
  | CPU
  | VCPU
  deriving (Eq, Ord)

data ComparisonBase
  = CTime
  | CData
  | CInstance
  | CIPAddress
  | CCPU
  | CVCPU
  deriving (Eq, Ord)

pBaseUOM :: Prism' Text BaseUOM
pBaseUOM = prism' pretty parse
  where pretty Second    = "s"
        pretty Hour      = "h"
        pretty Byte      = "B"
        pretty Instance  = "instance"
        pretty IPAddress = "ip-address"
        pretty CPU       = "cpu"
        pretty VCPU      = "vcpu-allocation"
        parse "s"               = Just Second
        parse "h"               = Just Hour
        parse "B"               = Just Byte
        parse "instance"        = Just Instance
        parse "ip-address"      = Just IPAddress
        parse "cpu"             = Just CPU
        parse "vcpu-allocation" = Just VCPU
        parse _                 = Nothing

pPrefixUOM :: Prism' Text Prefix
pPrefixUOM = prism' pretty parse
  where pretty Base = ""
        pretty Giga = "G"
        pretty Nano = "n"
        pretty Mebi = "Mi"
        pretty Mega = "M"
        parse ""   = Just Base
        parse "G"  = Just Giga
        parse "n"  = Just Nano
        parse "Mi" = Just Mebi
        parse "M"  = Just Mega
        parse _    = Nothing

pUOM :: Prism' Text UOM
pUOM = prism' pretty parse
  where dash = "-"
        pretty (u `Times` v) = (u ^. re pUOM) <> dash <> (v ^. re pUOM)
        pretty (UOM p b)     = (p ^. re pPrefixUOM) <> (b ^. re pBaseUOM)

        -- treat UOM @Times@ as left-associative.
        parse = hush . AT.parseOnly (parser <* AT.endOfInput)
        parser = do
          uoms <- puom `AT.sepBy` AT.string dash
          case uoms of []     -> mzero
                       -- construct left-associative UOMs
                       (u:us) -> return $ foldl Times u us
        puom = do
          pre  <- AT.option (Just Base) ppre
          base <- pbase
          case (pre, base) of (Just p, Just b)  -> return $ UOM p b
                              _                 -> mzero
        ppre   =  (preview pPrefixUOM <$> AT.string "G")
              <|> (preview pPrefixUOM <$> AT.string "n")
              <|> (preview pPrefixUOM <$> AT.string "Mi")
              <|> (preview pPrefixUOM <$> AT.string "M")
        pbase =   (preview pBaseUOM <$> AT.string "s")
              <|> (preview pBaseUOM <$> AT.string "h")
              <|> (preview pBaseUOM <$> AT.string "B")
              <|> (preview pBaseUOM <$> AT.string "instance")
              <|> (preview pBaseUOM <$> AT.string "ip-address")
              <|> (preview pBaseUOM <$> AT.string "cpu")
              <|> (preview pBaseUOM <$> AT.string "vcpu-allocation")

instance Show BaseUOM where
  show = T.unpack . review pBaseUOM

instance Show Prefix where
  show = T.unpack . review pPrefixUOM

instance Show UOM where
  show = T.unpack . review pUOM

instance Read UOM where
  readsPrec _ (T.pack -> x) = maybe [] (pure . (,"")) $ x ^? pUOM

instance FromJSON UOM where
  parseJSON (String t) = maybe mzero return $ t ^? pUOM

instance ToJSON UOM where
  toJSON x = String $ x ^. re pUOM


-- the following is copied from old borel code

reduce :: BaseUOM -> ComparisonBase
reduce Second    = CTime
reduce Hour      = CTime
reduce Byte      = CData
reduce Instance  = CInstance
reduce IPAddress = CIPAddress
reduce CPU       = CCPU
reduce VCPU      = CVCPU

prefixWeighting :: Prefix -> Double
prefixWeighting Base = 1
prefixWeighting Giga = 10^^(9 :: Int)
prefixWeighting Nano = 10^^(-9 :: Int)
prefixWeighting Mebi = 1024^^(2 :: Int)
prefixWeighting Mega = 10^^(6 :: Int)

baseWeighting :: BaseUOM -> Double
baseWeighting Hour = 60 * 60
baseWeighting _ = 1

weighting :: UOM -> Double
weighting (UOM p b) = prefixWeighting p * baseWeighting b
weighting (a `Times` b) = weighting a * weighting b

extractComparisonBases :: UOM -> MultiSet ComparisonBase
extractComparisonBases (UOM _ b) = S.singleton $ reduce b
extractComparisonBases (a `Times` b) = extractComparisonBases a <> extractComparisonBases b

convert :: UOM -> UOM -> Word64 -> Maybe Word64
convert old new v
  | extractComparisonBases old == extractComparisonBases new
  = let factor = weighting old / weighting new
    in  Just $ floor $ toRational factor * toRational v
  | otherwise = Nothing
