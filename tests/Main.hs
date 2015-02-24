{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
import Control.Monad
import           Control.Applicative
import Data.Monoid
import qualified Data.Set as S
import           Control.Lens
import           Control.Lens.Properties
import           Data.Word
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Function
import qualified Data.Bimap              as BM
import Data.Maybe
import Data.Either.Combinators
import Network.URI
import Data.Configurator

import Vaultaire.Types
import           Borel
import           Borel.Types
import           Borel.Types.Result
import           Borel.Types.UOM

import Debug.Trace

main :: IO ()
main = do
  hspec uomTest
  hspec confTest

uomTest :: Spec
uomTest = do

  describe "UOM conversion"  $
    prop  "converts nanosec to sec" $ property $ do
      uom <- arbitrary :: Gen UOM
      v   <- arbitrary :: Gen Word64
      let !lhs = _1 %~ flattenUOM $ nanosecToSec (uom,v)
      let !rhs = (mapped . filtered (==nanosec) .~ sec $ flattenUOM uom, v `div` 1000000000)
      return (lhs == rhs)

  describe "UOM pretty-printing" $ do
    it "as expected for basic UOMs"
      $ show uom0 `shouldBe` uomResult0
    it "as expected for compound UOMS"
      $ show uom1 `shouldBe` uomResult1

  describe "UOM parsing" $ do
    it "as expected for basic UOMs"
      $ (read uomResult0 :: UOM) `shouldBe` uom0
    it "as expected for compound UOMs"
      $ (read uomResult1 :: UOM) `shouldBe` uom1

uom0       = UOM Base Second
uomResult0 = "s"

uom1       = UOM Base Instance `Times` UOM Nano Second  `Times` UOM Giga Byte
uomResult1 = "instance-ns-GB"

confTest :: Spec
confTest =
  describe "Configuration parser" $
    it "parses the sample config" $ do
      c <- join $ parseBorelConfig <$> load [Required "tests/sample.conf"]
      c `shouldBe` Right sampleConf

sampleConf :: BorelConfig
sampleConf = 
  (mkBorelConfig (S.singleton $ fromRight' $ makeOrigin "ABCDEF")
                 undefined
                 (fromJust  $ parseURI   "tcp://example.com:999")
                 (fromJust  $ parseURI   "tcp://nsa.gov:3333")
                 (BM.fromList [("asio", 2866838636), ("koolaid", 944337339)]))

--------------------------------------------------------------------------------

instance Function UOM          where function = functionShow
instance Function Word64       where function = functionShow

instance Arbitrary Prefix  where arbitrary = arbitraryBoundedEnum
instance Arbitrary BaseUOM where arbitrary = arbitraryBoundedEnum
instance Arbitrary UOM     where arbitrary = sized someUOMs

instance CoArbitrary UOM where
  coarbitrary = variant . length . flattenUOM

someUOMs :: Int -> Gen UOM
someUOMs 0 = UOM <$> arbitrary <*> arbitrary
someUOMs n = do
  Positive m <- arbitrary `suchThat` ((<)n . getPositive)
  Positive p <- arbitrary `suchThat` ((<)n . getPositive)
  let x = n `div` m
      y = n `div` p
  left  <- someUOMs x
  right <- someUOMs y
  return (Times left right)

-- orphan instances for testing only, they don't make sense as actual uses.

instance Show BorelConfig where
  show (BorelConfig o _ u1 u2 fm ins ms)
    =   "config:("
    <> show o                    <> ","
    <> show u1                   <> ","
    <> show u2                   <> ","
    <> show fm                   <> ","
    <> show ins                  <> ","
    <> show ms                   <> ","
    <> ")"

instance Eq BorelConfig where
  (==) x y = show x == show y
