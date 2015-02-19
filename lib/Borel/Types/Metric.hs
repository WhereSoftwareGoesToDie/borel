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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Borel.Types.Metric
  ( -- * Metric
    Metric(..)
  , cpu, volumes, diskReads, diskWrites, neutronIn, neutronOut
  , ipv4, vcpus, memory, snapshot, image
  , mkInstance
  ) where

import           Data.Monoid
import           Data.Text        (Text)

import           Borel.Types.UOM
import           Ceilometer.Types


data Metric = Metric
    { deserialise :: Text -- ^ what it parses from
    , pretty      :: Text -- ^ what it pretty prints to
    , uom         :: UOM
    } deriving (Eq, Ord, Show)

mkInstance :: Flavor -> Metric
mkInstance name = Metric
  { deserialise      = "instances/" <> name
  , pretty           = "instance-"  <> name <> "-allocation"
  , uom              = UOM Base Instance `Times` nanosec
  }

cpu                  = Metric
  { deserialise      = "cpu"
  , pretty           = "cpu-usage"
  , uom              = UOM Base CPU `Times` nanosec
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
  , uom              = UOM Base IPAddress `Times` nanosec
  }

volumes              = Metric
  { deserialise      = "volumes"
  , pretty           = "volume-allocation"
  , uom              = gigabyte `Times` nanosec
  }

vcpus                = Metric
  { deserialise      = "vcpus"
  , pretty           = "vcpu-allocation"
  , uom              = UOM Base VCPU `Times` nanosec
  }

memory               = Metric
  { deserialise      = "memory"
  , pretty           = "memory-allocation"
  , uom              = megabyte `Times` nanosec
  }

snapshot             = Metric
  { deserialise      = "snapshot"
  , pretty           = "snapshot"
  , uom              = gigabyte `Times` nanosec
  }

image                = Metric
  { deserialise      = "image"
  , pretty           = "image"
  , uom              = byte `Times` nanosec
  }
