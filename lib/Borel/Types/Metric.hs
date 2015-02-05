-- | Copyright 2013-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--
-- /Description/
-- This module defines user-facing query-able metrics,
-- which can correspond to one or many Ceilometer resources.
--

{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Borel.Types.Metric
  ( -- * Metric
    Metric(..)
  , allMetrics
  , cpu, volumes, vcpus
  , mkInstance
  ) where

import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.Bimap            as BM
import           Data.Monoid
import           Data.Text (Text)
import           Web.Scotty            (Parsable, parseParam, readEither)

import           Vaultaire.Types
import           Ceilometer.Types
import           Borel.Types.UOM

data Metric = Metric
    { deserialise :: Text -- ^ what it parses from
    , pretty      :: Text -- ^ what it pretty prints to
    , uom         :: UOM
    } deriving (Eq, Ord, Show)

allMetrics :: FlavorMap -> [Metric]
allMetrics fm = allInstances fm <>
  [ diskReads, diskWrites
  , neutronIn, neutronOut
  , cpu, vcpus, memory, ipv4, volumes
  , snapshot, image
  ]

allInstances :: FlavorMap -> [Metric]
allInstances flavors = map mkInstance (BM.keys flavors)

mkInstance :: Flavor -> Metric
mkInstance name = Metric
  { deserialise      = "instances/" <> name
  , pretty           = "instance-"  <> name <> "-allocation"
  , uom              = UOM Base Instance `Times` nanoSec
  }

instanceExists :: FlavorMap -> Metric -> Bool
instanceExists fm x = any (==x) $ allInstances fm

cpu                  = Metric
  { deserialise      = "cpu"
  , pretty           = "cpu-usage"
  , uom              = UOM Base CPU `Times` nanoSec
  }

diskReads            = Metric
  { deserialise      = "diskio/reads"
  , pretty           = "diskio-reads"
  , uom              = byte
  }

diskWrites           = Metric
  { deserialise      = "diskio/writes"
  , pretty           = "diskio-writes"
  , uom              = byte
  }

neutronIn            = Metric
  { deserialise      = "neutron-traffic/incoming"
  , pretty           = "neutron-data-rx"
  , uom              = byte
  }

neutronOut           = Metric
  { deserialise      = "neutron-traffic/outgoing"
  , pretty           = "neutron-data-tx"
  , uom              = byte
  }

ipv4                 = Metric
  { deserialise      = "ipv4-addresses"
  , pretty           = "floating-ip-allocation"
  , uom              = UOM Base IPAddress `Times` nanoSec
  }

volumes              = Metric
  { deserialise      = "volumes"
  , pretty           = "volume-allocation"
  , uom              = gigabyte `Times` nanoSec
  }

vcpus                = Metric
  { deserialise      = "vcpus"
  , pretty           = "vcpu-allocation"
  , uom              = UOM Base VCPU `Times` nanoSec
  }

memory               = Metric
  { deserialise      = "memory"
  , pretty           = "memory-allocation"
  , uom              = megabyte `Times` nanoSec
  }

snapshot             = Metric
  { deserialise      = "snapshot"
  , pretty           = "snapshot"
  , uom              = gigabyte `Times` nanoSec
  }

image                = Metric
  { deserialise      = "image"
  , pretty           = "image"
  , uom              = byte `Times` nanoSec
  }

-- scotty

instance Parsable TimeStamp where
  parseParam = readEither

-- json

instance ToJSON UOM where
  toJSON x = toJSON $ show x

$(deriveJSON defaultOptions ''TimeStamp)
