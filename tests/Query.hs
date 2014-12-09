import           Control.Monad.Logger
import           Data.List            (nub, sort)
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
import           Vaultaire.Query      hiding (metrics)

main :: IO ()
main = hspec $ do
  -- safe
  describe "Ceilometer aggregation" $ do
    it "processes gauge data" $ pending
    it "processes cumulative data" $ pending
    it "processes event-based consolidated data" $ pending
    it "processes pollster-based consolidated data" $ pending

  describe "Result of a successful Borel query" $ do
    it "only includes the specified metrics" $ do
      xs <- runTest $ mkQuery metrics bang doom okGaugePoints
      let ms = sort $ nub $ map fst xs
      ms `shouldBe` metrics

  -- live

  where runTest (Select x)
          = runSafeT
          $ P.toListM
          $ runLoggerP LevelInfo
          $ runReaderP (BorelEnv conf bang doom) x

        conf = BackendConfig [] nullURI nullURI

        bang = TimeStamp 1300000000000000000
        doom = TimeStamp 1400000000000000000

        -- a Borel query only operates on metrics of the same group
        metrics = [ cpu ]


okGaugePoints , okCumulativePoints, okEventPoints, okPollsterPoints
  :: Monad m => Query m SimplePoint

okCumulativePoints = okGaugePoints
okGaugePoints      = Select $ each
  [ SimplePoint addr (TimeStamp 1378389600000000000) 1
  , SimplePoint addr (TimeStamp 1378389700000000000) 3
  , SimplePoint addr (TimeStamp 1378389800000000000) 5 ]
  where addr = read "LygHa16AHYF"

okEventPoints      = undefined
okPollsterPoints   = undefined
