{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Borel.Types
       ( -- * Entities
         -- disallow constructing arbitrary resources
         Resource, serialise, deserialise, group, domain, report, uom, pretty
       , cpu, diskReads, diskWrites, neutronIn, neutronOut
       , ipv4, volumes, vcpus, memory
       , allResources
       , parseResource
       , ResourceGroup(..), resourceGroups
       , Domain(..), domains, ResourceType(..), resourceTypes
       , Customer(..), Resources(..), Customers(..)
       , Metric(..), mkMetric
       , Quantity, UOM
       , ResourceIdentifier(..)
       , Format(..)
       , lookupResource
       , streamSummary
       )
where

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types
import qualified Data.ByteString.Char8 as B
import qualified Data.Csv              as C
import           Data.IP               (IPv4)
import           Data.Map              (Map)
import qualified Data.Map              as M
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.Lazy        as TL
import           Data.Word
import           Pipes
import qualified Pipes.Prelude         as P
import qualified Text.Read             as TR
import           Web.Scotty            (Parsable, parseParam, readEither)

import           Vaultaire.Types

type Quantity = Word64

data Resource = Resource
    { deserialise :: String -- ^ what it parses from
    , pretty      :: String -- ^ what it pretty prints to
    , uom         :: UOM
    , group       :: ResourceGroup
    } deriving (Eq, Ord)

instance Show Resource where
    show = pretty

domain :: ResourceGroup -> Domain
domain _         = OpenStack

report :: ResourceGroup -> ResourceType
report InstanceGroup = ConsolidatedPollster
report VCPUGroup = ConsolidatedPollster
report MemoryGroup = ConsolidatedPollster
report DiskReadGroup = Cumulative
report DiskWriteGroup = Cumulative
report NeutronInGroup = Cumulative
report NeutronOutGroup = Cumulative
report CPUGroup = Cumulative
report VolumeGroup = ConsolidatedEvent
report IPFloatingGroup = ConsolidatedEvent

cpu, diskReads, diskWrites, neutronIn, neutronOut :: Resource
ipv4, volumes :: Resource

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

allResources :: [Resource]
allResources = [ cpu, diskReads, diskWrites, neutronIn, neutronOut
               , volumes, vcpus, memory ]

instance Show ResourceGroup where
    show = serialise

serialise :: ResourceGroup -> String
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

data ResourceGroup = InstanceGroup
                   | DiskReadGroup
                   | DiskWriteGroup
                   | NeutronInGroup
                   | NeutronOutGroup
                   | VolumeGroup
                   | CPUGroup
                   | IPFloatingGroup
                   | VCPUGroup
                   | MemoryGroup
  deriving (Eq, Ord, Enum)

data Resources = AllResources | SomeResources [Resource]

data Customers = AllCustomers | SomeCustomers [Customer]

data ResourceType = Cumulative
                  | Gauge
                  | ConsolidatedPollster
                  | ConsolidatedEvent
  deriving (Show, Eq, Enum, Ord)

resourceGroups :: [ResourceGroup]
resourceGroups = [InstanceGroup ..]

resourceTypes :: [ResourceType]
resourceTypes = [Cumulative ..]

data Domain = OpenStack
  deriving (Eq, Enum, Show, Ord)

domains :: [Domain]
domains = [OpenStack ..]

data ResourceIdentifier
    = Tenancy { t_id :: String }
  deriving (Eq, Ord, Read, Show)

data Prefix
  = Base
  | Giga
  | Nano
  | Mebi
  | Mega
  deriving (Eq, Ord)

instance Show Prefix where
  show Base = ""
  show Giga = "G"
  show Nano = "n"
  show Mebi = "Mi"
  show Mega = "M"

prefixWeighting :: Prefix -> Double
prefixWeighting Base = 1
prefixWeighting Giga = 10^9
prefixWeighting Nano = 10^(-9)
prefixWeighting Mebi = 1024^2
prefixWeighting Mega = 10^6

data BaseUOM
  = Second
  | Byte
  | Instance
  | IPAddress
  | CPU
  | VCPU
  deriving (Eq, Ord)

instance Show BaseUOM where
  show Second    = "s"
  show Byte      = "B"
  show Instance  = "instance"
  show IPAddress = "ip-address"
  show CPU       = "cpu"
  show VCPU      = "vcpu-allocation"

data UOM
  = UOM Prefix BaseUOM
  | Times UOM UOM
  deriving (Eq, Ord)

instance Show UOM where
  show (u1 `Times` u2)   = concat [show u1, "-", show u2]
  show (UOM prefix base) = show prefix ++ show base

newtype Customer = Customer { cid :: Int }
  deriving (Parsable, Eq, Ord, Read)

instance Show Customer where
    show (Customer x) = show x

data Format = CSV | JSON | Summary

data Metric = Metric
    { metricCustomer   :: Customer
    , metricResource   :: Resource
    , metricUOM        :: UOM
    , metricQuantity   :: Quantity
    , metricIdentifier :: (Text, Maybe Text)
    } deriving Show

mkMetric :: Customer -> Resource -> Quantity -> (Text, Maybe Text) -> Metric
mkMetric c r = Metric c r (uom r)

-- | Typed version of @lookupSource@ to search and parse known key types.
lookup' :: Read a => String -> SourceDict -> Maybe a
lookup' k sd = TR.readMaybe =<< T.unpack <$> lookupSource (T.pack k) sd

lookupResource :: SourceDict -> Maybe ResourceIdentifier
lookupResource = lookup' "resource"


-- (De-)Serialisation ----------------------------------------------------------

parseResource :: String -> [Resource]
parseResource "diskio"          = [ diskReads, diskWrites ]
parseResource "neutron-traffic" = [ neutronIn, neutronOut ]
parseResource s = filter ((==s) . deserialise) allResources

instance Read Format where
  readsPrec _ s = (:[]) $ (,"") $ case s of
    "csv"     -> CSV
    "json"    -> JSON
    "summary" -> Summary

-- scotty
instance Parsable [Resource] where
  parseParam x = case parseResource (TL.unpack x) of
    [] -> Left $ TL.pack "resource list not parseable"
    y  -> Right y

instance Parsable Format where
  parseParam = readEither . TL.toLower
instance Parsable TimeStamp where
  parseParam = readEither

-- csv
instance C.ToField Resource where
  toField = B.pack . serialise . group
instance C.ToField UOM where
  toField = B.pack . show
instance C.ToField ResourceIdentifier where
  toField (Tenancy tid) = B.pack tid
instance C.ToField Customer where
  toField = B.pack . show . cid
instance C.ToRecord Metric where
  toRecord (Metric c r u v i)
    = C.record [ C.toField c, C.toField r, C.toField u , C.toField v,  C.toField (snd i)]

-- json
instance ToJSON Metric where
  toJSON (Metric c r u v (iK, iV))
    = object [ "resource" .= pretty r, "customer-id" .= c, "uom" .= u, "quantity" .= v, iK .= iV]

instance ToJSON UOM where
  toJSON x = toJSON $ show x

instance ToJSON Customer where
  toJSON (Customer c_id) = toJSON $ show c_id

summarise :: Monad m => Producer Metric m () -> m (Map (Customer, Resource, UOM) Quantity)
summarise = P.fold (\acc (Metric c r u q _) -> M.insertWith (+) (c, r, u) q acc) M.empty id

streamSummary :: Monad m => Producer Metric m () -> Producer B.ByteString m ()
streamSummary p = do
    summary <- lift $ summarise p
    mapM_ (\((c, r, u), q) -> yield $ summaryFormat c r u q) $ M.assocs summary

summaryFormat :: Customer -> Resource -> UOM -> Quantity -> B.ByteString
summaryFormat c r u q = B.pack $ concat [show $ cid c, ", ", pretty r, ", ", show u, ", ", show q, "\n"]

$(deriveJSON defaultOptions ''TimeStamp)
$(deriveJSON defaultOptions ''Format)
