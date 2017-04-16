--
-- Copyright © 2013-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--
-- * This module provides OS configuration
--   to be configured at run-time.
--
module Borel.Config

where

import Data.IxSet
import Borel.Types

-- | OpenStack resources configuration. This contains the enumeration of
--   supported resources and logical grouping (consult OpenStack documentation
--   for more information).
--
data OSConfig = OSConfig
  { _all                  :: IxSet Metric
  , _delta                :: IxSet Metric
  , _cumulative           :: IxSet Metric
  , _consolidatedPollster :: IxSet Metric
  , _consolidatedEvent    :: IxSet Metric }

mkOSConfig :: OSInstanceFlavours -> IO OSConfig
mkOSConfig = undefined

parseOSConfig :: FilePath -> IO OSInstanceFlavours
parseOSConfig = undefined

-- Supported OS resources ------------------------------------------------------

ipTx, ipRx :: Metric
diskReads, diskWrites             :: Metric
neutronIn, neutronOut             :: Metric
cpu, vcpus, memory, ipv4, volumes :: Metric
snapshot, image                   :: Metric

mkInstance :: String -> Metric
mkInstance name = Metric
  { _parse = "instances/" <> name
  , _print = "instance-" <> name <> "-allocation"
  , _uom = UOM Base Instance `Times` UOM Nano Second
  }

ipTx = Metric
  { _parse = "ip-data-tx"
  , _print = "ip-data-tx"
  , _uom = UOM Base Byte
  }

ipRx = Metric
  { _parse = "ip-data-rx"
  , _print = "ip-data-rx"
  , _uom = UOM Base Byte
  }

cpu = Metric
  { _parse = "cpu"
  , _print = "cpu-usage"
  , _uom = UOM Base CPU `Times` UOM Nano Second
  }

diskReads = Metric
  { _parse = "diskio/reads"
  , _print = "diskio-reads"
  , _uom = UOM Base Byte
  }

diskWrites = Metric
  { _parse = "diskio/writes"
  , _print = "diskio-writes"
  , _uom = UOM Base Byte
  }

neutronIn = Metric
  { _parse = "neutron-traffic/incoming"
  , _print = "neutron-data-rx"
  , _uom = UOM Base Byte
  }

neutronOut = Metric
  { _parse = "neutron-traffic/outgoing"
  , _print = "neutron-data-tx"
  , _uom = UOM Base Byte
  }

ipv4 = Metric
  { _parse = "ipv4-addresses"
  , _print = "floating-ip-allocation"
  , _uom = UOM Base IPAddress `Times` UOM Nano Second
  }

volumes = Metric
  { _parse = "volumes"
  , _print = "volume-allocation"
  , _uom = UOM Giga Byte `Times` UOM Nano Second
  }

vcpus = Metric
  { _parse  = "vcpus"
  , _print  = "vcpu-allocation"
  , _uom = UOM Base VCPU `Times` UOM Nano Second
  }

memory = Metric
  { _parse  = "memory"
  , _print  = "memory-allocation"
  , _uom = UOM Mega Byte `Times` UOM Nano Second
  }

snapshot = Metric
  { _parse = "snapshot"
  , _print  = "snapshot"
  , _uom = UOM Giga Byte `Times` UOM Nano Second
  }

image = Metric
  { _parse = "image"
  , _print = "image"
  , _uom = UOM Base Byte `Times` UOM Nano Second
  }
