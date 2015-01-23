{-# LANGUAGE OverloadedStrings #-}

import           Borel.Aggregate
import           Control.Monad.Logger
import           Data.Bits
import           Data.List            (nub, sort)
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Monoid
import           Data.Word
import           Network.URI
import           Pipes
import           Pipes.Lift
import qualified Pipes.Prelude        as P
import           Pipes.Safe
import           Test.Hspec

import           Borel.Log
import           Borel.Query
import           Borel.Types
import           Marquise.Types
import           Vaultaire.Query

main :: IO ()
main = hspec $ do
  -- safe

  describe "Ceilometer aggregation" $ do
    it "processes gauge data" $ pending
    it "processes cumulative data" $ pending
    it "processes event-based consolidated data" $ do
        xs <- runTest $ mkQuery volMetrics s e okEventPoints
        xs `shouldBe` [(volumes, expectedEventPayload)]
    it "processes pollster-based consolidated data" $ do
        xs <- runTest $ mkQuery instanceMetrics 0 15 okPollsterPoints
        let ms = sort $ nub xs
        ms `shouldBe` expectedInstanceResult

-- = 7 seconds of flavor 2 + 4 seconds of flavor 3

  describe "Result of a successful Borel query" $ do
    it "only includes the specified metrics" $ do
      xs <- runTest $ mkQuery metrics s e okGaugePoints
      let ms = sort $ nub $ map fst xs
      ms `shouldBe` metrics

  -- live

  where runTest (Select x)
          = runSafeT
          $ P.toListM
          $ runLoggerP LevelInfo
          $ runReaderP (BorelEnv conf flavorMap s e) x

        conf = BackendConfig [] nullURI nullURI

        -- a Borel query only operates on metrics of the same group
        metrics = [ cpu ]
        volMetrics = [ volumes ]
        instanceMetrics = map computeInstance $ M.elems flavorMap

s, e :: TimeStamp
s = TimeStamp 1300000000000000000
e = TimeStamp 1400000000000000000
sRaw, eRaw :: Word64
sRaw = unTimeStamp s
eRaw = unTimeStamp e

okGaugePoints , okCumulativePoints, okEventPoints, okPollsterPoints
  :: Monad m => Query m SimplePoint

okCumulativePoints = okGaugePoints
okGaugePoints      = Select $ each
  [ SimplePoint addr (TimeStamp 1378389600000000000) 1
  , SimplePoint addr (TimeStamp 1378389700000000000) 3
  , SimplePoint addr (TimeStamp 1378389800000000000) 5 ]


addr = read "LygHa16AHYF"


-- Volume events
-- Expected = 2 * 10 + 5 * 30 + 4 * 30 + 10 * 10
--          = 20 + 150 + 120 + 100
--          = 390
okEventPoints      = Select $ each
  [ SimplePoint addr (TimeStamp sRaw) vPayload1
  , SimplePoint addr (TimeStamp (sRaw + 2)) vPayload2
  , SimplePoint addr (TimeStamp (sRaw + 7)) vPayload2
  , SimplePoint addr (TimeStamp (sRaw + 11)) vPayload1
  , SimplePoint addr (TimeStamp (sRaw + 21)) vPayloadEnd
  ]

expectedEventPayload = 390

vPayload1, vPayload2 :: Word64
vPayload1 = 2 + (1 `shift` 8) + (2 `shift` 16) + (10 `shift` 32)
vPayload2 = 2 + (1 `shift` 8) + (2 `shift` 16) + (30 `shift` 32)
vPayloadEnd = 4 + (3 `shift` 8) + (2 `shift` 16) + (30 `shift` 32)

flavorMap :: Map Word32 String
flavorMap = M.fromList [ (siphash "1", "flavor1")
                       , (siphash "2", "flavor2")
                       , (siphash "3", "flavor3")
                       ]

-- Instance pollsters
-- Expected 2 seconds of flavor2 + 5 seconds of flavor 2 + 4 seconds of flavor 3 + 2 seconds of flavor 2
-- = 9 seconds of flavor 2 + 4 seconds of flavor 3
okPollsterPoints   = Select $ each
  [ SimplePoint addr (TimeStamp 0)  (siphash "2")
  , SimplePoint addr (TimeStamp 2)  (siphash "2")
  , SimplePoint addr (TimeStamp 7)  (siphash "3")
  , SimplePoint addr (TimeStamp 11) (siphash "2")
  , SimplePoint addr (TimeStamp 13) (siphash "2")
  ]

expectedInstanceResult =
    [ (computeInstance "flavor1", 0)
    , (computeInstance "flavor2", 9)
    , (computeInstance "flavor3", 4)
    ]
