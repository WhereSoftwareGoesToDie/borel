{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative
import           Control.Lens
import           Control.Lens.Properties
import           Data.Word
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Function

import           Borel
import           Borel.Types.Result
import           Borel.Types.UOM

import Debug.Trace

main :: IO ()
main = do
  hspec uomTest

uomTest :: Spec
uomTest = do
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

  describe "UOM conversion"  $
    prop  "converts nanosec to sec" $ property $ do
      uom <- arbitrary
      v   <- arbitrary :: Gen Word64
      let lhs = _1 %~ flattenUOM $ nanosecToSec (uom,v)
          rhs = (traversed . filtered (==nanosec) .~ sec $ (flattenUOM uom), v `div` 1000000000)
      return $ lhs == rhs

uom0       = UOM Base Second
uomResult0 = "s"

uom1       = UOM Base Instance `Times` UOM Nano Second  `Times` UOM Giga Byte
uomResult1 = "instance-ns-GB"


--------------------------------------------------------------------------------

instance Function UOM          where function = functionShow
instance Function ResponseItem where function = functionShow
instance Function Word64       where function = functionShow

instance Arbitrary Prefix  where arbitrary = arbitraryBoundedEnum
instance Arbitrary BaseUOM where arbitrary = arbitraryBoundedEnum
instance Arbitrary UOM     where arbitrary = sized someUOMs
instance Arbitrary ResponseItem where
  arbitrary = ResponseItem "" "" <$> arbitrary <*> arbitrary

instance CoArbitrary UOM where
  coarbitrary = variant . length . flattenUOM

instance CoArbitrary ResponseItem where
  coarbitrary (ResponseItem _ _ u v)
    = variant v

someUOMs :: Int -> Gen UOM
someUOMs 0 = UOM <$> arbitrary <*> arbitrary
someUOMs n = do
  Positive m <- arbitrary
  Positive p <- arbitrary
  let x = n `div` m
      y = n `div` p
  left  <- someUOMs x
  right <- someUOMs y
  return (Times left right)
