{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Function
import Data.Word

import           Borel
import           Borel.Types.UOM
import           Borel.Types.Result
import           Properties

main :: IO ()
main = hspec uomTest

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

uom0       = UOM Base Second
uomResult0 = "s"

uom1       = UOM Base Instance `Times` UOM Nano Second  `Times` UOM Giga Byte
uomResult1 = "instance-ns-GB"

resultTest :: Spec
resultTest =
  describe "Response UOM-value traversal" $
    prop "is a proper traversal" $ isTraversal traverseUOMVal


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
    = variant (length $ flattenUOM u)
    . variant v
    

someUOMs :: Int -> Gen UOM
someUOMs 0 = UOM <$> arbitrary <*> arbitrary
someUOMs n = do
  NonNegative m <- arbitrary
  NonNegative n <- arbitrary
  left  <- someUOMs m
  right <- someUOMs n
  return (Times left right)
