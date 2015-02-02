{-# LANGUAGE RecordWildCards #-}

module Borel.Aggregate.Ceilometer.Consolidated.Types where

import           Data.Word

data Payload = ComputeInstance String
             | IPAlloc
             | Volume Word32
             | Memory Word32
             | VCpu   Word32
             | Snapshot Word32
             | Image    Word32
  deriving (Ord, Eq, Show)

-- |A decoded SimplePoint sans Address
data ConsolidatedPoint =
    EventPoint
        { eventPayload  :: Payload
        , eventReserved :: Word8
        , eventEndpoint :: EventEndpoint
        , eventVerb     :: EventVerb
        , eventStatus   :: EventStatus
        , eventTime     :: Word64
        } |
    PollsterPoint
        { pollsterPayload :: Payload
        , pollsterTime    :: Word64
        }
    deriving Show

extractPayload :: ConsolidatedPoint -> Payload
extractPayload EventPoint{..}    = eventPayload
extractPayload PollsterPoint{..} = pollsterPayload

extractTime :: ConsolidatedPoint -> Word64
extractTime EventPoint{..}    = eventTime
extractTime PollsterPoint{..} = pollsterTime

data InstanceStatus = AnyStatus
    deriving (Eq, Show)

-- |The status of the resource at the time, resource specific
data EventStatus = IPAllocStatus IPAllocStatus
                 | VolumeStatus VolumeStatus
                 | SnapshotStatus SnapshotStatus
                 | InstanceStatus InstanceStatus
    deriving (Eq, Show)

data IPAllocStatus = IPAllocNoStatus
                   | IPAllocActive
                   | IPAllocDown
    deriving (Eq, Show)

data VolumeStatus = VolumeError
                  | VolumeAvailable
                  | VolumeCreating
                  | VolumeExtending
                  | VolumeDeleting
                  | VolumeAttaching
                  | VolumeDetaching
                  | VolumeInUse
    deriving (Eq, Show)

data SnapshotStatus = SnapshotError
                    | SnapshotAvailable
                    | SnapshotCreating
                    | SnapshotDeleting
    deriving (Eq, Show)

-- |The action that is happening, e.g. Instance Creation or Shutdown
-- |0 is reserved for events which should not ever happen
data EventVerb = Apocalypse
               | IPAllocVerb IPAllocVerb
               | VolumeVerb VolumeVerb
               | SnapshotVerb SnapshotVerb
    deriving (Eq, Show)

data IPAllocVerb = IPAllocCreate
                 | IPAllocUpdate
                 | IPAllocDelete
    deriving (Eq, Show)

data VolumeVerb = VolumeCreate
                | VolumeResize
                | VolumeDelete
                | VolumeAttach
                | VolumeDetach
    deriving (Eq, Show)

data SnapshotVerb = SnapshotCreate
                  | SnapshotUpdate
                  | SnapshotDelete
    deriving (Eq, Show)

-- |The endpoint represents whether the data represents the start of an event, the
--  end of an event, or whether the event is treated as instant
data EventEndpoint = Instant
                   | Start
                   | End
    deriving (Eq, Show)
