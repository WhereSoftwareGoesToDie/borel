{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- * Borel resource types
--
module Borel.Types.Resource
  ( -- * Resource
    Resource(..)
  , ResourceGroup(..), ResMeasure(..)
  , UOM, BaseUOM, Prefix
    -- * Enumeration
  , resourceGroups, resourceMeasures
  , osResources
    -- * Pre-defined OpenStack Resources
  , cpu, diskReads, diskWrites, neutronIn, neutronOut
  , instanceM1Tiny, instanceM1Small, instanceM1Medium, instanceM1Large, instanceM1XLarge
  , ipv4, volumes, vcpus, memory
    -- * Mappings
  , report, serialise, prefixWeighting
  ) where

import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString.Char8 as B
import qualified Data.Csv              as C
import           Web.Scotty            (Parsable, parseParam, readEither)

import           Vaultaire.Types


-- | Resource attribute: logical groups
--
data ResourceGroup
  = InstanceGroup
  | IPTxGroup
  | IPRxGroup
  | DiskReadGroup
  | DiskWriteGroup
  | NeutronInGroup
  | NeutronOutGroup
  | VolumeGroup
  | CPUGroup
  | IPFloatingGroup
  | VCPUGroup
  | MemoryGroup
  | ImageGroup
  | SnapshotGroup
  deriving (Eq, Ord, Enum)

-- | Resource attribute: how it is measured and reported by OpenStack.
--
data ResMeasure
  = Cumulative
  | Gauge
  | ConsolidatedPollster
  | ConsolidatedEvent
  deriving (Show, Eq, Enum, Ord)

-- | Resource attribute: unit of measurement
--
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
  | Byte
  | Instance
  | IPAddress
  | CPU
  | VCPU
  deriving (Eq, Ord)

data Resource = Resource
    { deserialise :: String -- ^ what it parses from
    , pretty      :: String -- ^ what it pretty prints to
    , uom         :: UOM
    , group       :: ResourceGroup
    } deriving (Eq, Ord)


resourceGroups :: [ResourceGroup]
resourceGroups = [InstanceGroup ..]

resourceMeasures :: [ResMeasure]
resourceMeasures = [Cumulative ..]


-- Mappings between resource attributes ----------------------------------------

prefixWeighting :: Prefix -> Double
prefixWeighting Base = 1
prefixWeighting Giga = 10^(9 :: Int)
prefixWeighting Nano = 10^(-9 :: Int)
prefixWeighting Mebi = 1024^(2 :: Int)
prefixWeighting Mega = 10^(6 :: Int)

report :: ResourceGroup -> ResMeasure
report IPTxGroup       = Gauge
report IPRxGroup       = Gauge
report InstanceGroup   = ConsolidatedPollster
report VCPUGroup       = ConsolidatedPollster
report MemoryGroup     = ConsolidatedPollster
report DiskReadGroup   = Cumulative
report DiskWriteGroup  = Cumulative
report NeutronInGroup  = Cumulative
report NeutronOutGroup = Cumulative
report CPUGroup        = Cumulative
report VolumeGroup     = ConsolidatedEvent
report IPFloatingGroup = ConsolidatedEvent
report ImageGroup      = Gauge
report SnapshotGroup   = ConsolidatedEvent


-- OpenStack Resources ---------------------------------------------------------

osResources :: [Resource]
osResources =
  [ cpu, diskReads, diskWrites, neutronIn, neutronOut
  , instanceM1Tiny, instanceM1Small, instanceM1Medium, instanceM1Large, instanceM1XLarge
  , volumes, vcpus, memory ]

diskReads, diskWrites             :: Resource
neutronIn, neutronOut             :: Resource
cpu, vcpus, memory, ipv4, volumes :: Resource
instanceM1Tiny, instanceM1Small, instanceM1Medium, instanceM1Large, instanceM1XLarge :: Resource

cpu = Resource
  { deserialise = "cpu"
  , pretty = "cpu-usage"
  , uom = UOM Base CPU `Times` UOM Nano Second
  , group  = CPUGroup
  }

diskReads = Resource
  { deserialise = "diskio/reads"
  , pretty = "diskio-reads"
  , uom = UOM Base Byte
  , group  = DiskReadGroup
  }

diskWrites = Resource
  { deserialise = "diskio/writes"
  , pretty = "diskio-writes"
  , uom = UOM Base Byte
  , group  = DiskWriteGroup
  }

neutronIn = Resource
  { deserialise = "neutron-traffic/incoming"
  , pretty = "neutron-data-rx"
  , uom = UOM Base Byte
  , group  = NeutronInGroup
  }

neutronOut = Resource
  { deserialise = "neutron-traffic/outgoing"
  , pretty = "neutron-data-tx"
  , uom = UOM Base Byte
  , group  = NeutronOutGroup
  }

ipv4 = Resource
  { deserialise = "ipv4-addresses"
  , pretty = "floating-ip-allocations"
  , uom = UOM Base IPAddress
  , group  = IPFloatingGroup
  }

instanceM1Tiny = Resource
  { deserialise = "instances/m1-tiny"
  , pretty = "instance-tiny-allocation"
  , uom = UOM Base Instance `Times` UOM Nano Second
  , group  = InstanceGroup
  }

instanceM1Small = Resource
  { deserialise = "instances/m1-small"
  , pretty = "instance-small-allocation"
  , uom = UOM Base Instance `Times` UOM Nano Second
  , group  = InstanceGroup
  }

instanceM1Medium = Resource
  { deserialise = "instances/m1-medium"
  , pretty = "instance-medium-allocation"
  , uom = UOM Base Instance `Times` UOM Nano Second
  , group  = InstanceGroup
  }

instanceM1Large = Resource
  { deserialise = "instances/m1-large"
  , pretty = "instance-large-allocation"
  , uom = UOM Base Instance `Times` UOM Nano Second
  , group  = InstanceGroup
  }

instanceM1XLarge = Resource
  { deserialise = "instances/m1-xlarge"
  , pretty = "instance-xlarge-allocation"
  , uom = UOM Base Instance `Times` UOM Nano Second
  , group  = InstanceGroup
  }

volumes = Resource
  { deserialise = "volumes"
  , pretty = "volume-allocation"
  , uom = UOM Giga Byte `Times` UOM Nano Second
  , group  = VolumeGroup
  }

vcpus = Resource
  { deserialise  = "vcpus"
  , pretty  = "vcpu-allocation"
  , uom = UOM Base VCPU `Times` UOM Nano Second
  , group = VCPUGroup
  }

memory = Resource
  { deserialise  = "memory"
  , pretty  = "memory-allocation"
  , uom = UOM Mega Byte `Times` UOM Nano Second
  , group = MemoryGroup
  }


-- (De)-Serialistion -----------------------------------------------------------

serialise :: ResourceGroup -> String
serialise IPTxGroup       = "tx"
serialise IPRxGroup       = "rx"
serialise CPUGroup        = "cpu"
serialise DiskReadGroup   = "disk.read.bytes"
serialise DiskWriteGroup  = "disk.write.bytes"
serialise NeutronInGroup  = "network.incoming.bytes"
serialise NeutronOutGroup = "network.outgoing.bytes"
serialise IPFloatingGroup = "ip.floating"
serialise InstanceGroup   = "instance_flavor"
serialise VolumeGroup     = "volumes"
serialise VCPUGroup       = "instance_vcpus"
serialise MemoryGroup     = "instance_ram"
serialise ImageGroup      = "image.size"
serialise SnapshotGroup   = "snapshot.size"

-- show/read

instance Show ResourceGroup where
    show = serialise

instance Show Prefix where
  show Base = ""
  show Giga = "G"
  show Nano = "n"
  show Mebi = "Mi"
  show Mega = "M"

instance Show BaseUOM where
  show Second    = "s"
  show Byte      = "B"
  show Instance  = "instance"
  show IPAddress = "ip-address"
  show CPU       = "cpu"
  show VCPU      = "vcpu-allocation"

instance Show UOM where
  show (u1 `Times` u2)   = concat [show u1, "-", show u2]
  show (UOM prefix base) = show prefix ++ show base

instance Show Resource where
    show = pretty

-- scotty

instance Parsable TimeStamp where
  parseParam = readEither

-- csv

instance C.ToField Resource where
  toField = B.pack . serialise . group
instance C.ToField UOM where
  toField = B.pack . show

-- json

instance ToJSON UOM where
  toJSON x = toJSON $ show x

$(deriveJSON defaultOptions ''TimeStamp)
