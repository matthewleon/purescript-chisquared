module Test.Math.Statistics.Test.ChiSquared.Table
  ( tableSpec
  ) where

import Prelude
import Math.Statistics.Test.ChiSquared.Table as Table
import Data.Array ((..), all, elem)
import Data.Enum (class Enum, enumFromTo)
import Data.Maybe (Maybe(..), fromJust)
import Data.Traversable (traverse)
import Partial.Unsafe (unsafePartial)

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldContain)

import Test.Math.Statistics.Test.ChiSquared.StringTable as StringTable

tableSpec :: forall r. Spec r Unit
tableSpec =
  describe "Math.Statistic.Test.ChiSquared.Table" do
    it "supports certain alpha values" do
      shouldContain enumerate (Table.alpha 0.001)
      shouldContain enumerate (Table.alpha 0.01)
      shouldContain enumerate (Table.alpha 0.025)
      shouldContain enumerate (Table.alpha 0.05)
      shouldContain enumerate (Table.alpha 0.1)

    it "supports degrees of freedom between 1 and 100" do
      shouldEqual
        (all (flip maybeElem enumerate) (Table.degreesOfFreedom <$> 1 .. 100))
        true

    it "corresponds to NIST values" do
      lookups `shouldEqual` StringTable.array

    where
      enumerate :: forall t. (Enum t, Bounded t) => Array t
      enumerate = enumFromTo bottom top

      maybeElem :: forall t. Eq t => Maybe t -> Array t -> Boolean
      maybeElem (Just x) xs = elem x xs
      maybeElem Nothing  _  = false

      lookups :: Array (Array Number)
      lookups =
        let dfs = unsafePartial $ fromJust $
                  traverse Table.degreesOfFreedom (1 .. 100)
            alphas = unsafePartial $ fromJust $
                     traverse Table.alpha [0.1, 0.05, 0.025, 0.01, 0.001]
            cvs = fCombine Table.lookup dfs alphas
        in  map (map Table.cvToNumber) cvs

      pairMap :: forall t1 t2 a b. (Functor t1, Functor t2) =>
        t1 (a -> b) -> t2 a -> t1 (t2 b)
      pairMap fs xs = ((<#>) xs) <$> fs

      fCombine :: forall t1 t2 a b c. (Functor t1, Functor t2) =>
        (a -> b -> c) -> t1 a -> t2 b -> t1 (t2 c)
      fCombine f xs = pairMap (f <$> xs)
