{-# LANGUAGE RecordWildCards #-}

module Borel.Resource.Ceilometer.Consolidated.Types where

import           Data.Word

data Payload = IPAlloc
             | Volume Word32
             | Memory Word64
             | VCpu   Word64
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

-- |The status of the resource at the time, resource specific
data EventStatus = IPAllocStatus IPAllocStatus
                 | VolumeStatus VolumeStatus
    deriving (Eq, Show)

data IPAllocStatus = IPAllocActive
                   | IPAllocDown
    deriving (Eq, Show)

data VolumeStatus = VolumeAvailable
                  | VolumeCreating
                  | VolumeExtending
                  | VolumeDeleting
    deriving (Eq, Show)

-- |The action that is happening, e.g. Instance Creation or Shutdown
-- |0 is reserved for events which should not ever happen
data EventVerb = Apocalypse
               | IPAllocVerb IPAllocVerb
               | VolumeVerb VolumeVerb
    deriving (Eq, Show)

data IPAllocVerb = IPAllocCreate
                 | IPAllocUpdate
    deriving (Eq, Show)

data VolumeVerb = VolumeCreate
                | VolumeResize
                | VolumeDelete
    deriving (Eq, Show)


-- |The endpoint represents whether the data represents the start of an event, the
--  end of an event, or whether the event is treated as instant
data EventEndpoint = Instant
                   | Start
                   | End
    deriving (Eq, Show)
