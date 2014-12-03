{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeOperators     #-}

module Borel.Aggregate.Ceilometer.Consolidated.Parse where

import           Control.Monad.Logger
import           Crypto.MAC.SipHash                     (SipHash (..),
                                                         SipKey (..), hash)
import           Data.Binary
import           Data.Binary.Get
import           Data.ByteString                        (ByteString)
import           Pipes
import qualified Pipes.Prelude                          as P

import           Marquise.Types
import           Vaultaire.Query

import           Borel.Types
import           Borel.Aggregate.Ceilometer.Consolidated.Types
import           Borel.Log

-- |Reserved byte is treated as raw data
decodeReserved :: Word8 -> Word8
decodeReserved = Prelude.id

decodeEndpoint :: Word8 -> EventEndpoint
decodeEndpoint 0 = Instant
decodeEndpoint 1 = Start
decodeEndpoint 2 = End
decodeEndpoint x = error $ "Unsupported event endpoint " ++ show x

-- |0 is reserved for events which should not ever happen
decodeVerb :: MetricGroup -> Word8 -> EventVerb
decodeVerb IPFloatingGroup x
    | x == 1    = IPAllocVerb IPAllocCreate
    | x == 2    = IPAllocVerb IPAllocUpdate
    | x == 3    = IPAllocVerb IPAllocDelete
    | otherwise = error $ "Unsupporrted IP allocation verb " ++ show x
decodeVerb VolumeGroup x
    | x == 1    = VolumeVerb VolumeCreate
    | x == 2    = VolumeVerb VolumeResize
    | x == 3    = VolumeVerb VolumeDelete
    | x == 4    = VolumeVerb VolumeAttach
    | x == 5    = VolumeVerb VolumeDetach
    | otherwise = error $ "Unsupported volume verb " ++ show x
decodeVerb SnapshotGroup x
    | x == 1    = SnapshotVerb SnapshotCreate
    | x == 2    = SnapshotVerb SnapshotUpdate
    | x == 3    = SnapshotVerb SnapshotDelete
    | otherwise = error $ "Unsupported snapshot verb " ++ show x
decodeVerb r _  = error $ concat ["Resource ", show r, " does not support verbs"]

decodeStatus :: MetricGroup -> Word8 -> EventStatus
decodeStatus IPFloatingGroup x
    | x == 0     = IPAllocStatus IPAllocNoStatus
    | x == 1     = IPAllocStatus IPAllocActive
    | x == 2     = IPAllocStatus IPAllocDown
    | otherwise  = error $ "Unsupported IP allocation status " ++ show x
decodeStatus VolumeGroup x
    | x == 0     = VolumeStatus VolumeError
    | x == 1     = VolumeStatus VolumeAvailable
    | x == 2     = VolumeStatus VolumeCreating
    | x == 3     = VolumeStatus VolumeExtending
    | x == 4     = VolumeStatus VolumeDeleting
    | x == 5     = VolumeStatus VolumeAttaching
    | x == 6     = VolumeStatus VolumeDetaching
    | x == 7     = VolumeStatus VolumeInUse
    | otherwise  = error $ "Unsupport volume status " ++ show x
decodeStatus SnapshotGroup x
    | x == 0     = SnapshotStatus SnapshotError
    | x == 1     = SnapshotStatus SnapshotAvailable
    | x == 2     = SnapshotStatus SnapshotCreating
    | x == 3     = SnapshotStatus SnapshotDeleting
    | otherwise  = error $ "Unsupport snapshot status " ++ show x
decodeStatus r _ = error $ concat ["Resource ", show r, " does not support statuses"]

decodeEventPayload :: MetricGroup -> Word32 -> Maybe Payload
decodeEventPayload InstanceGroup x = decodeEventInstancePayload x
decodeEventPayload x y = decodePollsterPayload x (fromIntegral y)

decodePollsterPayload :: MetricGroup -> Word64 -> Maybe Payload
decodePollsterPayload InstanceGroup   x = decodePollsterInstancePayload x
decodePollsterPayload IPFloatingGroup _ = Just IPAlloc
decodePollsterPayload VolumeGroup     x = Just $ Volume $ fromIntegral x
decodePollsterPayload VCPUGroup       x = Just $ VCpu $ fromIntegral x
decodePollsterPayload MemoryGroup     x = Just $ Memory $ fromIntegral x
decodePollsterPayload _ _ = Nothing

decodeEventInstancePayload :: Word32 -> Maybe Payload
decodeEventInstancePayload 6  = Just M1Tiny
decodeEventInstancePayload 15 = Just M1Small
decodeEventInstancePayload 3  = Just M1Medium
decodeEventInstancePayload 9  = Just M1Large
decodeEventInstancePayload 12 = Just M1XLarge
decodeEventInstancePayload _  = Nothing

decodePollsterInstancePayload :: Word64 -> Maybe Payload
decodePollsterInstancePayload x
    | x == siphash "1" = Just M1Tiny
    | x == siphash "2" = Just M1Small
    | x == siphash "3" = Just M1Medium
    | x == siphash "4" = Just M1Large
    | x == siphash "5" = Just M1XLarge
    | otherwise        = Nothing

siphash :: ByteString -> Word64
siphash x = let (SipHash h) = hash (SipKey 0 0) x in h

billableVerb :: MetricGroup -> EventVerb -> Bool
billableVerb IPFloatingGroup _      = True
billableVerb VolumeGroup     (VolumeVerb x)
    | x == VolumeCreate = True
    | x == VolumeResize = True
    | x == VolumeDelete = False
billableVerb _ _ = False

-- |1st 4 MSBytes represent the payload
--  5th   MSByte  is reserved
--  6th   MSByte  represents the endpoint
--  7th   MSByte  represents the verb
--  8th   MSByte  represents the resolution
parseEvent :: MetricGroup
           -> Word64
           -> Word64
           -> Maybe ConsolidatedPoint
parseEvent rGroup ts bytes = let (a, b, c, d, e) = gets in do
  payload <- decodeEventPayload rGroup a
  reserve <- Just $ decodeReserved b
  ep      <- Just $ decodeEndpoint c
  verb    <- Just $ decodeVerb rGroup d
  status  <- Just $ decodeStatus rGroup e
  return $ EventPoint payload reserve ep verb status ts
  where gets :: (Word32, Word8, Word8, Word8, Word8)
        gets = flip runGet (encode bytes) $ do
                 a <- getWord32be
                 b <- getWord8
                 c <- getWord8
                 d <- getWord8
                 e <- getWord8
                 return (a, b, c, d, e)

parsePollster :: MetricGroup
              -> Word64
              -> Word64
              -> Maybe ConsolidatedPoint
parsePollster rGroup ts bytes = case decodePollsterPayload rGroup (fromIntegral bytes) of
    Just p  -> Just $ PollsterPoint p ts
    Nothing -> Nothing

parseConsolidated :: (MonadLogger m)
                  => MetricGroup
                  -> Query m SimplePoint
                  -> Query m ConsolidatedPoint
parseConsolidated rGroup (Select points)
  = let (f, str) = case report rGroup of
                       ConsolidatedEvent    -> (parseEvent, serialise rGroup ++ " event")
                       ConsolidatedPollster -> (parsePollster, serialise rGroup ++ " pollster")
                       _                    -> error $ concat ["Attempted to process non-consolidated resource ", serialise $ rGroup, " as consolidated"]
    in Select $ points >-> P.map (\p@(SimplePoint _ (TimeStamp ts) bytes)
                               -> (p, f rGroup ts bytes))
                       >-> yieldOrWarn str
  where yieldOrWarn str = for cat $ \x -> case x of
                        (_, Just y)  -> yield y
                        (SimplePoint addr t bytes, Nothing)
                          -> logInfoStr $ concat [ "parseConsolidated: cannot parse point as ", str, ": payload = "
                                                 , show bytes, ", timestamp = ", show t, " addr = ", show addr]
