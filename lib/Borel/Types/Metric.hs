{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- * Borel resource types
--
module Borel.Types.Metric
  ( -- * Metric
    Metric(..)
  , MetricGroup(..), ResourceMeasure(..)
  , UOM(..), BaseUOM(..), Prefix(..), convert
    -- * Enumeration
  , resourceGroups, resourceMeasures
  , allMetrics
    -- * Pre-defined resources
  , ipTx, ipRx, snapshot
  , cpu, diskReads, diskWrites, neutronIn, neutronOut
  , computeInstance, ipv4, volumes, vcpus, memory
    -- * Mappings
  , report, serialise, prefixWeighting, FlavorMap
  ) where

import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString.Char8 as B
import qualified Data.Csv              as C
import           Data.Map              (Map)
import qualified Data.Map              as M
import           Data.Monoid
import           Data.MultiSet         (MultiSet)
import qualified Data.MultiSet         as S
import           Data.Word
import           Web.Scotty            (Parsable, parseParam, readEither)

import           Vaultaire.Types

-- | A mapping of flavor_id hashes to flavor_names
type FlavorMap = Map Word32 String

-- | Metric attribute: logical groups
--
data MetricGroup
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

-- | Metric attribute: how it is measured and reported by OpenStack.
--
data ResourceMeasure
  = Cumulative
  | Delta
  | ConsolidatedPollster
  | ConsolidatedEvent
  deriving (Show, Eq, Enum, Ord)

-- | Metric attribute: unit of measurement
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

reduce :: BaseUOM -> ComparisonBase
reduce Second    = CTime
reduce Hour      = CTime
reduce Byte      = CData
reduce Instance  = CInstance
reduce IPAddress = CIPAddress
reduce CPU       = CCPU
reduce VCPU      = CVCPU

data Metric = Metric
    { deserialise :: String -- ^ what it parses from
    , pretty      :: String -- ^ what it pretty prints to
    , uom         :: UOM
    , group       :: MetricGroup
    } deriving (Eq, Ord)


resourceGroups :: [MetricGroup]
resourceGroups = [InstanceGroup ..]

resourceMeasures :: [ResourceMeasure]
resourceMeasures = [Cumulative ..]


-- Mappings between resource attributes ----------------------------------------

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

coercible :: UOM -> UOM -> Bool
coercible x y = extractComparisonBases x == extractComparisonBases y

convert :: UOM -> UOM -> Word64 -> Maybe Word64
convert oldUOM newUOM v =
    if coercible oldUOM newUOM then
        let factor = weighting oldUOM / weighting newUOM in
            Just $ floor $ toRational factor * toRational v
        else
            Nothing

-- | How to interpret simple points for a metric group
--
report :: MetricGroup -> ResourceMeasure
report IPTxGroup       = Delta
report IPRxGroup       = Delta
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
report ImageGroup      = ConsolidatedPollster
report SnapshotGroup   = ConsolidatedEvent

--Resources --------------------------------------------------------------------

allMetrics :: FlavorMap -> [Metric]
allMetrics flavors = map computeInstance (M.elems flavors) <>
  [ diskReads, diskWrites
  , neutronIn, neutronOut
  , cpu, vcpus, memory, ipv4, volumes
  , snapshot, image
  ]

ipTx, ipRx :: Metric
diskReads, diskWrites             :: Metric
neutronIn, neutronOut             :: Metric
cpu, vcpus, memory, ipv4, volumes :: Metric
snapshot, image                   :: Metric

ipTx = Metric
  { deserialise = "ip-data-tx"
  , pretty = "ip-data-tx"
  , uom = UOM Base Byte
  , group  = IPTxGroup
  }

ipRx = Metric
  { deserialise = "ip-data-rx"
  , pretty = "ip-data-rx"
  , uom = UOM Base Byte
  , group  = IPRxGroup
  }

cpu = Metric
  { deserialise = "cpu"
  , pretty = "cpu-usage"
  , uom = UOM Base CPU `Times` UOM Nano Second
  , group  = CPUGroup
  }

diskReads = Metric
  { deserialise = "diskio/reads"
  , pretty = "diskio-reads"
  , uom = UOM Base Byte
  , group  = DiskReadGroup
  }

diskWrites = Metric
  { deserialise = "diskio/writes"
  , pretty = "diskio-writes"
  , uom = UOM Base Byte
  , group  = DiskWriteGroup
  }

neutronIn = Metric
  { deserialise = "neutron-traffic/incoming"
  , pretty = "neutron-data-rx"
  , uom = UOM Base Byte
  , group  = NeutronInGroup
  }

neutronOut = Metric
  { deserialise = "neutron-traffic/outgoing"
  , pretty = "neutron-data-tx"
  , uom = UOM Base Byte
  , group  = NeutronOutGroup
  }

ipv4 = Metric
  { deserialise = "ipv4-addresses"
  , pretty = "floating-ip-allocation"
  , uom = UOM Base IPAddress `Times` UOM Nano Second
  , group  = IPFloatingGroup
  }

computeInstance :: String -> Metric
computeInstance name = Metric
  { deserialise = "instances/" <> name
  , pretty = "instance-" <> name <> "-allocation"
  , uom = UOM Base Instance `Times` UOM Nano Second
  , group  = InstanceGroup
  }

volumes = Metric
  { deserialise = "volumes"
  , pretty = "volume-allocation"
  , uom = UOM Giga Byte `Times` UOM Nano Second
  , group  = VolumeGroup
  }

vcpus = Metric
  { deserialise  = "vcpus"
  , pretty  = "vcpu-allocation"
  , uom = UOM Base VCPU `Times` UOM Nano Second
  , group = VCPUGroup
  }

memory = Metric
  { deserialise  = "memory"
  , pretty  = "memory-allocation"
  , uom = UOM Mega Byte `Times` UOM Nano Second
  , group = MemoryGroup
  }

snapshot = Metric
  { deserialise = "snapshot"
  , pretty  = "snapshot"
  , uom = UOM Giga Byte `Times` UOM Nano Second
  , group = SnapshotGroup
  }

image = Metric
  { deserialise = "image"
  , pretty = "image"
  , uom = UOM Base Byte `Times` UOM Nano Second
  , group = ImageGroup
  }

-- (De)-Serialistion -----------------------------------------------------------

serialise :: MetricGroup -> String
serialise IPTxGroup       = "tx"
serialise IPRxGroup       = "rx"
serialise CPUGroup        = "cpu"
serialise DiskReadGroup   = "disk.read.bytes"
serialise DiskWriteGroup  = "disk.write.bytes"
serialise NeutronInGroup  = "network.incoming.bytes"
serialise NeutronOutGroup = "network.outgoing.bytes"
serialise IPFloatingGroup = "ip.floating"
serialise InstanceGroup   = "instance_flavor"
serialise VolumeGroup     = "volume.size"
serialise VCPUGroup       = "instance_vcpus"
serialise MemoryGroup     = "instance_ram"
serialise ImageGroup      = "image.size"
serialise SnapshotGroup   = "snapshot.size"

-- show/read

instance Show MetricGroup where
    show = serialise

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

instance Show Metric where
    show = pretty

-- scotty

instance Parsable TimeStamp where
  parseParam = readEither

-- csv

instance C.ToField Metric where
  toField = B.pack . serialise . group
instance C.ToField UOM where
  toField = B.pack . show

-- json

instance ToJSON UOM where
  toJSON x = toJSON $ show x

$(deriveJSON defaultOptions ''TimeStamp)
