module Math.Statistics.Test.ChiSquared.Pearson.Buckets
  ( Buckets
  , BucketsConstructorError(..)
  , buckets
  , fromArray
  , df
  , toNonEmptyArray
  , Bucket(..)
  , bucket
  , count
  , expectation
  , Count(..)
  , Expectation(..)
  , discreteUniformBucketize
  , uniformBucketize
  ) where

import Prelude
import Data.Array (uncons, fromFoldable, length, zipWith, replicate, modifyAt)
import Data.Either (Either(..), fromRight)
import Data.Enum (class BoundedEnum, Cardinality, cardinality, fromEnum)
import Data.Int (toNumber, floor)
import Data.Foldable (class Foldable, foldl, minimum, maximum)
import Data.Maybe (Maybe(..), maybe', fromMaybe', fromJust)
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)

import Math.Statistics.Test.ChiSquared.Table (
  DegreesOfFreedom, degreesOfFreedom
)

type AtLeastTwo f t = NonEmpty (NonEmpty f) t
data Buckets = Buckets (AtLeastTwo Array Bucket) DegreesOfFreedom
instance showBuckets :: Show Buckets where
  show (Buckets bs dof) = "Buckets " <> show bs <> " " <> show dof
derive instance eqBuckets :: Eq Buckets

data BucketsConstructorError = UnsupportedSize Int
instance showBucketsConstructorError :: Show BucketsConstructorError where
  show (UnsupportedSize i) = "UnsupportedSize " <> show i
derive instance eqBucketsConstructorError :: Eq BucketsConstructorError

buckets :: forall f. Foldable f =>
  Bucket -> Bucket -> f Bucket -> Either BucketsConstructorError Buckets
buckets x1 x2 xs =
  let bucketArray = fromFoldable xs
      attemptedBuckets = x1 :| x2 :| bucketArray
      bucketCount = 2 + length bucketArray
      maybeDegreesOfFreedom = degreesOfFreedom (bucketCount - 1)
      err _ = Left $ UnsupportedSize bucketCount
  in maybe' err (Right <<< Buckets attemptedBuckets) maybeDegreesOfFreedom

fromArray :: Array Bucket -> Either BucketsConstructorError Buckets
fromArray arr = fromMaybe' err maybeBuckets
  where
    err _ = Left $ UnsupportedSize (length arr)
    maybeBuckets = do
      {head : x1, tail: xs} <- uncons arr
      {head : x2, tail: xs'} <- uncons xs
      pure $ buckets x1 x2 xs'

toNonEmptyArray :: Buckets -> AtLeastTwo Array Bucket
toNonEmptyArray (Buckets bs _) = bs

df :: Buckets -> DegreesOfFreedom
df (Buckets _ dof) = dof

newtype Bucket = Bucket (Tuple Count Expectation)
instance showBucket :: Show Bucket where
  show (Bucket b) = "Bucket " <> show b
derive instance newtypeBucket :: Newtype Bucket _
derive newtype instance eqBucket :: Eq Bucket
derive newtype instance ordBucket :: Ord Bucket

bucket :: Int -> Number -> Bucket
bucket c e = Bucket (Tuple (Count c) (Expectation e))

count :: Bucket -> Int
count (Bucket (Tuple (Count c) _)) = c

expectation :: Bucket -> Number
expectation (Bucket (Tuple _ (Expectation e))) = e

newtype Count = Count Int
instance showCount :: Show Count where
  show (Count i) = "Count " <> show i
derive instance newtypeCount :: Newtype Count _
derive newtype instance eqCount :: Eq Count
derive newtype instance ordCount :: Ord Count
derive newtype instance semiringCount :: Semiring Count

newtype Expectation = Expectation Number
instance showExpectation :: Show Expectation where
  show (Expectation n) = "Expectation " <> show n
derive instance newtypeExpectation :: Newtype Expectation _
derive newtype instance eqExpectation :: Eq Expectation
derive newtype instance ordExpectation :: Ord Expectation
derive newtype instance semiringExpectation :: Semiring Expectation
derive newtype instance euclidianRingExpectation :: EuclideanRing Expectation

discreteUniformBucketize :: forall f t. (Foldable f, BoundedEnum t) =>
  NonEmpty f t -> Buckets
discreteUniformBucketize observedValues =
  unsafePartial $ fromRight $
    fromArray $ zipWith bucket bucketedObservations expectations
  where
    bucketCount = unwrap (cardinality :: Cardinality t)
    expectations = replicate bucketCount expectation'
    expectation' = (toNumber <<< length $ fromFoldable observedValues) /
                   toNumber bucketCount

    bucketedObservations = foldl go initialObservationArray observedValues
    initialObservationArray = replicate bucketCount 0
    bucketIndex obs = fromEnum obs - fromEnum (bottom :: t)

    go :: Array Int -> t -> Array Int
    -- TODO: use ST
    go cs obs = unsafePartial $ fromJust $
      modifyAt (bucketIndex obs) (add one) cs

uniformBucketize :: forall f. Foldable f => NonEmpty f Number -> Buckets
-- iterates through the foldable to establish its range
uniformBucketize observedValues =
  uniformBucketize' observedValues 100 $
    unsafePartial $ fromJust $
      join $ range <$> minimum observedValues <*> maximum observedValues

newtype Range = Range (Tuple Number Number)
instance showRange :: Show Range where
  show (Range (Tuple l h)) = "range " <> show l <> " " <> show h
derive instance eqRange :: Eq Range

low :: Range -> Number
low (Range (Tuple l _)) = l

high :: Range -> Number
high (Range (Tuple _ h)) = h

range :: Number -> Number -> Maybe Range
range x y = if x <= y then Just (Range $ Tuple x y) else Nothing

rangeSize :: Range -> Number
rangeSize r = high r - low r

uniformBucketize' :: forall f. Foldable f =>
  NonEmpty f Number -> Int -> Range -> Buckets
-- if any of the observed values fall outside of the range, they will
-- be placed in the first or last bucket,
uniformBucketize' observedValues bucketCount bucketRange =
  unsafePartial $ fromRight $
    fromArray $ zipWith bucket bucketedObservations expectations
  where
    expectations = replicate bucketCount expectation'
    expectation' = (toNumber <<< length $ fromFoldable observedValues) /
                   toNumber bucketCount

    bucketedObservations = foldl go initialObservationArray observedValues
    initialObservationArray = replicate bucketCount 0
    bucketSize  = rangeSize bucketRange / toNumber bucketCount
    bucketIndex observation =
      clamp 0 (bucketCount - 1) <<< floor $
        (observation - low bucketRange) / bucketSize

    go :: Array Int -> Number -> Array Int
    -- TODO: use ST
    go cs n = unsafePartial $ fromJust $ modifyAt (bucketIndex n) (add one) cs
