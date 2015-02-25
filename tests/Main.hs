{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative
import           Control.Lens             hiding (elements)
import           Control.Lens.Properties
import           Control.Monad
import qualified Data.Bimap               as BM
import           Data.Configurator
import           Data.Either.Combinators
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set                 as S
import qualified Data.List                as L
import qualified Data.Text                as T
import           Data.Word
import           Network.URI
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Function
import qualified Data.Aeson as A

import           Borel
import           Borel.Types
import           Borel.Types.Result
import           Borel.Types.UOM
import           Vaultaire.Types

import           Debug.Trace

main :: IO ()
main = do
  hspec uomTest
  hspec confTest
  hspec respTest

uomTest :: Spec
uomTest = do

  describe "UOM conversion"  $
    prop  "converts nanosec to sec" $ property $ do
      uom <- uomNoNanosec
      return $ trace ("uom=" ++ show uom ) $ nanosecToSec (uom `Times` nanosec, 1000000000) == (uom `Times` sec, 1)
 
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

confTest :: Spec
confTest =
  describe "Configuration parser" $
    it "parses the sample config" $ do
      c <- join $ parseBorelConfig <$> load [Required "tests/sample.conf"]
      c `shouldBe` Right sampleConf


respTest :: Spec
respTest = do
  describe "Response Item" $
    it "has a valid Setter for (UOM, Value)" $ isSetter setUOMVal

  describe "Response Item JSON" $
    prop "x == decode (encode x)" $ property $ do
      x <- arbitrary :: Gen ResponseItem
      return $ Just x == A.decode (A.encode x)

--------------------------------------------------------------------------------

uom0       = UOM Base Second
uomResult0 = "s"

uom1       = UOM Base Instance `Times` UOM Nano Second  `Times` UOM Giga Byte
uomResult1 = "instance-ns-GB"

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
instance Function ResponseItem where function = functionShow

instance Arbitrary Prefix  where arbitrary = arbitraryBoundedEnum
instance Arbitrary BaseUOM where arbitrary = arbitraryBoundedEnum
instance Arbitrary UOM     where arbitrary = sized someUOMs
instance Arbitrary ResponseItem where
  arbitrary =   ResponseItem "" ""
            <$> arbitrary
            <*> arbitrary

instance CoArbitrary UOM where
  coarbitrary = variant . length . flattenUOM

instance CoArbitrary ResponseItem where
  coarbitrary (ResponseItem _ _ u v)
    = variant (length $ flattenUOM u)
    . variant v

-- Our print/parse UOM is only total for UOMs made up of
-- these UOMs.
supportedUOMs :: Gen UOM
supportedUOMs
  = elements
    [ sec, nanosec, byte, megabyte, gigabyte
    , countCPU, countVCPU, countInstance, countIP ]

someUOMs :: Int -> Gen UOM
someUOMs 0 = supportedUOMs
someUOMs n = do
  Positive m <- arbitrary `suchThat` ((<)n . getPositive)
  Positive p <- arbitrary `suchThat` ((<)n . getPositive)
  let x = n `div` m
      y = n `div` p
  left  <- someUOMs x
  right <- someUOMs y
  return (Times left right)

uomNoNanosec :: Gen UOM
uomNoNanosec = arbitrary `suchThat` (not . any (== nanosec) . flattenUOM)

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
