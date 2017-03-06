module Test.Math.Statistics.Test.ChiSquared.Pearson.Buckets
  ( bucketsSpec
  , CoinFlips(..)
  , CoinFlip(..)
  ) where

import Prelude
import Control.Monad.Eff.Random (RANDOM)
import Data.Array (filter, length, fromFoldable)
import Data.Either (Either(..), isRight)
import Data.Enum (class Enum, class BoundedEnum, Cardinality(..), toEnum)
import Data.Foldable (all, sum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.Number (eqApproximate)
import Partial.Unsafe (unsafePartial)

import Test.QuickCheck ((===))
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, chooseInt, vectorOf)

import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)

import Math.Statistics.Test.ChiSquared.Pearson.Buckets (
  BucketsConstructorError(..), buckets, fromArray, toNonEmptyArray, df
, Bucket, bucket, count, expectation
, discreteUniformBucketize, uniformBucketize
)

bucketsSpec :: forall r. Spec (random :: RANDOM | r) Unit
bucketsSpec =
  describe "Math.Statistics.Test.ChiSquared.Pearson.Buckets" do

    describe "buckets constructor" do
      it "succeeds when provided with <= 101 buckets" $
        quickCheck \(ArbBucket b1) (ArbBucket b2) (UpTo99Buckets bs) ->
          isRight $ buckets b1 b2 bs
      it "fails with a constructor error when provided with > 101 buckets" $
        quickCheck \(ArbBucket b1) (ArbBucket b2) (Over99Buckets bs) ->
          buckets b1 b2 bs === Left (UnsupportedSize (2 + length bs))

    describe "fromArray" do
      it "fails with a constructor error when provided with < 2 buckets" $
        quickCheck \(FewerThan2Buckets bs) ->
          fromArray bs === Left (UnsupportedSize $ length bs)
      it "succeeds when provided with between 2 and 101 buckets" $
        quickCheck \(Between2And101Buckets bs) -> isRight $ fromArray bs
      it "fails with a constructor error when provided with > 101 buckets" $
        quickCheck \(Over101Buckets bs) ->
          fromArray bs === Left (UnsupportedSize $ length bs)

    describe "toNonEmptyArray" do
      it "returns an array of the buckets used in construction" do
        quickCheck \(ArbBucket b1) (ArbBucket b2) (UpTo99Buckets bs) ->
          toNonEmptyArray <$> buckets b1 b2 bs
          === Right (b1 :| b2 :| bs)

    describe "df" do
      it "returns the correct degrees of freedom" $
        quickCheck \(Between2And101Buckets bs) ->
          unwrap <<< df <$> fromArray bs
          === Right (length bs - 1)

    describe "discreteUniformBucketize" do
      it "distributes into the correct buckets" $
        quickCheck \(CoinFlips coinFlips) ->
          (toNonEmptyArray $ discreteUniformBucketize coinFlips)
            ===
              bucket (length $ filter ((==) Heads) $ fromFoldable coinFlips)
                     ((toNumber $ length (fromFoldable coinFlips)) / 2.0)
              :| bucket (length $ filter ((==) Tails) $ fromFoldable coinFlips)
                        ((toNumber $ length (fromFoldable coinFlips)) / 2.0)
              :| []
      it "creates a maximum of 100 buckets" $
        quickCheck \(NonEmptyIntArray xs) ->
          -- Int is a weird BoundedEnum instance with Cardinality -1
          (length $ fromFoldable $ toNonEmptyArray $ discreteUniformBucketize xs)
          === 100

    describe "uniformBucketize" do
      it "buckets every observation" do
        quickCheck \(Observations observations@(o :| os)) ->
          sum (count <$> (toNonEmptyArray $ uniformBucketize observations))
          === 1 + length os
      it "sets all bucket expectations properly" do
        quickCheck \(Observations observations@(o :| os)) ->
          all
            (eqApproximate $ toNumber (1 + length os) / 100.0)
            (expectation <$> (toNonEmptyArray $ uniformBucketize observations))

newtype ArbBucket = ArbBucket Bucket
derive instance newtypeArbBucket :: Newtype ArbBucket _
instance arbitraryArbBucket :: Arbitrary ArbBucket where
  arbitrary = ArbBucket <$> (bucket <$> arbitrary <*> arbitrary)

newtype UpTo99Buckets = UpTo99Buckets (Array Bucket)
instance aribtraryUpTo99Buckets :: Arbitrary UpTo99Buckets where
  arbitrary = UpTo99Buckets <$> map unwrap <$>
    sizeRangeVectorOf 0 99 (arbitrary :: Gen ArbBucket)

newtype Over99Buckets = Over99Buckets (Array Bucket)
instance aribtraryOver99Buckets :: Arbitrary Over99Buckets where
  arbitrary = Over99Buckets <$> map unwrap <$>
    sizeRangeVectorOf 100 10000 (arbitrary :: Gen ArbBucket)

newtype FewerThan2Buckets = FewerThan2Buckets (Array Bucket)
instance aribtraryFewerThan2Buckets :: Arbitrary FewerThan2Buckets where
  arbitrary = FewerThan2Buckets <$> map unwrap <$>
    sizeRangeVectorOf 0 1 (arbitrary :: Gen ArbBucket)

newtype Between2And101Buckets = Between2And101Buckets (Array Bucket)
instance aribtraryBetween2And101Buckets :: Arbitrary Between2And101Buckets where
  arbitrary = Between2And101Buckets <$> map unwrap <$>
    sizeRangeVectorOf 2 101 (arbitrary :: Gen ArbBucket)

newtype Over101Buckets = Over101Buckets (Array Bucket)
instance aribtraryOver101Buckets :: Arbitrary Over101Buckets where
  arbitrary = Over101Buckets <$> map unwrap <$>
    sizeRangeVectorOf 102 10000 (arbitrary :: Gen ArbBucket)

sizeRangeVectorOf :: forall a. Int -> Int -> Gen a -> Gen (Array a)
sizeRangeVectorOf min max g = do
  actualSize <- chooseInt min max
  minArray <- vectorOf min g
  additionalArray <- vectorOf (actualSize - min) g
  pure (minArray <> additionalArray)

newtype Observations = Observations (NonEmpty Array Number)
instance arbitraryObservations :: Arbitrary Observations where
  arbitrary = Observations <$> (NonEmpty <$> arbitrary <*> arbitrary)

newtype CoinFlips = CoinFlips (NonEmpty Array CoinFlip)
instance arbitraryCoinFlips :: Arbitrary CoinFlips where
  arbitrary = CoinFlips <$> (NonEmpty <$> arbitrary <*> arbitrary)

data CoinFlip = Heads | Tails
instance showCoinFlip :: Show CoinFlip where
  show Heads = "Heads"
  show Tails = "Tails"
derive instance eqCoinFlip :: Eq CoinFlip
derive instance ordCoinFlip :: Ord CoinFlip
instance boundedCoinFlip :: Bounded CoinFlip where
  bottom = Heads
  top = Tails
instance enumCoinFlip :: Enum CoinFlip where
  succ Heads = Just Tails
  succ Tails = Nothing
  pred Heads = Nothing
  pred Tails = Just Heads
instance boundedEnumCoinFlip :: BoundedEnum CoinFlip where
  fromEnum Heads = 0
  fromEnum Tails = 1
  toEnum 0 = Just Heads
  toEnum 1 = Just Tails
  toEnum _ = Nothing
  cardinality = Cardinality 2
instance arbitraryCoinFlip :: Arbitrary CoinFlip where
  arbitrary = do
    n <- chooseInt 0 1
    pure (unsafePartial $ fromJust $ toEnum n)

newtype NonEmptyIntArray = NonEmptyIntArray (NonEmpty Array Int)
instance arbitraryNonEmptyIntArray :: Arbitrary NonEmptyIntArray where
  arbitrary = NonEmptyIntArray <$> (NonEmpty <$> arbitrary <*> arbitrary)
