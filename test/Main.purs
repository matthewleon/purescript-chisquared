module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM)

import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

import Test.Math.Statistics.Test.ChiSquared.Table (tableSpec)
import Test.Math.Statistics.Test.ChiSquared.Pearson (pearsonSpec)
import Test.Math.Statistics.Test.ChiSquared.Pearson.Buckets (bucketsSpec)

main :: forall r. Eff (RunnerEffects (random :: RANDOM | r)) Unit
main = run [consoleReporter] do
  tableSpec
  bucketsSpec
  pearsonSpec
