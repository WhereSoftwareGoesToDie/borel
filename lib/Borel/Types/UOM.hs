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

module Borel.Types.UOM
  ( -- * Unit of measurement
    UOM(..), BaseUOM(..), Prefix(..)
    -- * Convenient constructors
  , nanoSec, byte, megabyte, gigabyte
    -- * Utilities
  , convert
  ) where

import           Data.Monoid
import           Data.MultiSet (MultiSet)
import qualified Data.MultiSet as S
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

instance Show Prefix where
  show Base = ""
  show Giga = "G"
  show Nano = "n"
  show Mebi = "Mi"
  show Mega = "M"

instance Show BaseUOM where
  show Second    = "s"
  show Hour      = "h"
  show Byte      = "B"
  show Instance  = "instance"
  show IPAddress = "ip-address"
  show CPU       = "cpu"
  show VCPU      = "vcpu-allocation"

instance Show UOM where
  show (u1 `Times` u2)   = concat [show u1, "-", show u2]
  show (UOM prefix base) = show prefix ++ show base

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
