module Math.Statistics.Test.ChiSquared.Pearson
  ( TestResult(..)
  , didPass
  , pearsonTest
  , ChiSquaredStat(..)
  , chiSquaredStat
  , module Math.Statistics.Test.ChiSquared.Pearson.Buckets
  , module Math.Statistics.Test.ChiSquared.Table
  ) where

import Prelude
import Data.Foldable (sum)
import Data.Int (toNumber)
import Data.NonEmpty (NonEmpty)
import Data.Tuple (Tuple(..))

import Math.Statistics.Test.ChiSquared.Pearson.Buckets (
  Buckets, buckets, toNonEmptyArray, df,
  Bucket(..), bucket,
  Count(..), Expectation(..)
)
import Math.Statistics.Test.ChiSquared.Table (
  Alpha, alpha,
  DegreesOfFreedom, degreesOfFreedom,
  CriticalValue, cvToNumber,
  lookup
)

newtype ChiSquaredStat = ChiSquaredStat Number
derive newtype instance showChiSquaredStat :: Show ChiSquaredStat
derive newtype instance eqChiSquaredStat :: Eq ChiSquaredStat

data TestResult = Accept | Reject
instance showTestResult :: Show TestResult where
  show Accept = "Accept"
  show Reject = "Reject"
derive instance eqTestResult :: Eq TestResult
didPass :: TestResult -> Boolean
didPass Accept = true
didPass Reject = false

pearsonTest :: Alpha -> Buckets -> TestResult
pearsonTest alpha bs = result (lookup (df bs) alpha) (chiSquaredStat bs)
  where
    result :: CriticalValue -> ChiSquaredStat -> TestResult
    result cv (ChiSquaredStat stat) =
      if stat <= (cvToNumber cv) then Accept else Reject

chiSquaredStat :: Buckets -> ChiSquaredStat
chiSquaredStat = ChiSquaredStat <<< sum <<< contributions
  where
    contributions :: Buckets -> NonEmpty (NonEmpty Array) Number
    contributions bs = map contribution (toNonEmptyArray bs)

    contribution :: Bucket -> Number
    contribution (Bucket (Tuple (Count obs) (Expectation exp))) =
      (square (toNumber obs - exp)) / exp

    square x = x * x
