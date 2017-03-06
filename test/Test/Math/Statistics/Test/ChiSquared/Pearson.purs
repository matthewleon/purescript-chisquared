module Test.Math.Statistics.Test.ChiSquared.Pearson
  ( pearsonSpec
  ) where

import Prelude
import Control.Monad.Eff.Random (RANDOM)
import Data.Array (replicate)
import Data.Either (fromRight)
import Data.Foldable (all)
import Data.Maybe (Maybe(..), fromJust)
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (unfoldr)
import Partial.Unsafe (unsafePartial)

import Test.QuickCheck ((===))
import Test.QuickCheck.Gen (vectorOf)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)

import Test.Math.Statistics.Test.ChiSquared.Pearson.Buckets (CoinFlips(..))

import Math.Statistics.Test.ChiSquared.Pearson (
  TestResult(..), pearsonTest, Alpha, alpha
, ChiSquaredStat(..), chiSquaredStat
, Buckets, buckets, bucket
)

import Math.Statistics.Test.ChiSquared.Pearson.Buckets (
  discreteUniformBucketize, uniformBucketize, uniformBucketize'
, Range, range
)

pearsonSpec :: forall r. Spec (random :: RANDOM | r) Unit
pearsonSpec =
  describe "Math.Statistics.Test.ChiSquared.Pearson" do
    describe "chiSquaredStat" do
      it "calculates correct chi squared stats" do
        chiSquaredStat (buckets' (bucket 1 1.0) (bucket 1 1.0) [])
          `shouldEqual` ChiSquaredStat 0.0
        chiSquaredStat (buckets' (bucket 2 2.0) (bucket 1 1.0) [])
          `shouldEqual` ChiSquaredStat 0.0
        chiSquaredStat (buckets' (bucket 2 1.0) (bucket 5 5.0) [])
           `shouldEqual` ChiSquaredStat 1.0
        chiSquaredStat (buckets' (bucket 10 1.0) (bucket 1 1.0) [])
           `shouldEqual` ChiSquaredStat 81.0
    describe "pearsonTest" do
      it "passes clearly uniform data" do
        (alwaysPasses $ uniformBucketize uniformData) `shouldEqual` true
      it "fails clearly non-uniform data" do
        (alwaysFails $ uniformBucketize biasedData) `shouldEqual` true
      it "passes random coin flips with permissive alpha" do
        quickCheck \(CoinFlips coinFlips) ->
          pearsonTest permissiveAlpha (discreteUniformBucketize coinFlips)
          === Accept
    describe "pearsontest" do
      it "passes quickCheck's uniform generator with permissive alpha" do
        quickCheck \(LotsOfNums nums) ->
          pearsonTest permissiveAlpha (uniformBucketize' nums 100 uniformRange)
          === Accept
      it ("fails quickCheck's uniform generator with permissive alpha"
          <> "if the range is incorrectly set") do
        quickCheck \(LotsOfNums nums) ->
          pearsonTest permissiveAlpha (uniformBucketize' nums 100 falseRange)
          === Reject
  where
    buckets' b1 b2 bs = unsafePartial $ fromRight $ buckets b1 b2 bs

    alwaysPasses :: Buckets -> Boolean
    alwaysPasses bs =
      all ((==) Accept) $ alphas <#> \alpha -> pearsonTest alpha bs

    alwaysFails :: Buckets -> Boolean
    alwaysFails bs =
      all ((==) Reject) $ alphas <#> \alpha -> pearsonTest alpha bs

    alphas :: Array Alpha
    alphas = unsafePartial $ fromJust $
             traverse alpha [0.1, 0.05, 0.025, 0.01, 0.001]

    permissiveAlpha :: Alpha
    permissiveAlpha = unsafePartial $ fromJust $ alpha 0.001

    uniformData :: NonEmpty Array Number
    uniformData = 1.0 :| unfoldr
      (\n -> if n > 2.0 then Nothing else Just (Tuple (n + 0.0001) (n + 0.0001)))
      1.0

    biasedData :: NonEmpty Array Number
    biasedData = 1.0 :| replicate 1000 2.0

    uniformRange :: Range
    uniformRange = unsafePartial $ fromJust $ range 0.0 1.0

    falseRange :: Range
    falseRange = unsafePartial $ fromJust $ range 0.0 1.1

newtype LotsOfNums = LotsOfNums (NonEmpty Array Number)
instance arbitraryLotsOfNums :: Arbitrary LotsOfNums where
  arbitrary = LotsOfNums <$> (NonEmpty <$> arbitrary <*> vectorOf 100000 arbitrary)
