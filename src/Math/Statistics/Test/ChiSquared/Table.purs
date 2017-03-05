module Math.Statistics.Test.ChiSquared.Table (
  Alpha,
  alpha,
  DegreesOfFreedom,
  degreesOfFreedom,
  CriticalValue,
  cvToNumber,
  lookup
) where

import Prelude
import Data.Array (index)
import Data.Enum (class Enum, defaultPred, defaultSucc,
                  class BoundedEnum, defaultCardinality)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Partial.Unsafe (unsafePartial)

data Alpha = Alpha001 | Alpha01 | Alpha025 | Alpha05 | Alpha1
instance showAlpha :: Show Alpha where
  show a = "alpha " <> show (alphaToNumber a)
derive instance eqAlpha :: Eq Alpha
instance ordAlpha :: Ord Alpha where
  compare = comparing alphaToNumber
instance boundedAlpha :: Bounded Alpha where
  bottom = Alpha001
  top    = Alpha1
instance enumAlpha :: Enum Alpha where
  pred = defaultPred alphaToEnum alphaFromEnum
  succ = defaultSucc alphaToEnum alphaFromEnum
instance boundedEnumAlpha :: BoundedEnum Alpha where
  fromEnum = alphaFromEnum
  toEnum = alphaToEnum
  cardinality = defaultCardinality

alpha :: Number -> Maybe Alpha
alpha 0.1    = Just $ Alpha1
alpha 0.05   = Just $ Alpha05
alpha 0.025  = Just $ Alpha025
alpha 0.01   = Just $ Alpha01
alpha 0.001  = Just $ Alpha001
alpha _      = Nothing

alphaToNumber :: Alpha -> Number
alphaToNumber Alpha1   = 0.1
alphaToNumber Alpha05  = 0.05
alphaToNumber Alpha025 = 0.025
alphaToNumber Alpha01  = 0.01
alphaToNumber Alpha001 = 0.001

alphaFromEnum :: Alpha -> Int
alphaFromEnum Alpha001 = 0
alphaFromEnum Alpha01  = 1
alphaFromEnum Alpha025 = 2
alphaFromEnum Alpha05  = 3
alphaFromEnum Alpha1   = 4

alphaToEnum :: Int -> Maybe Alpha
alphaToEnum 0 = Just Alpha001
alphaToEnum 1 = Just Alpha01
alphaToEnum 2 = Just Alpha025
alphaToEnum 3 = Just Alpha05
alphaToEnum 4 = Just Alpha1
alphaToEnum _ = Nothing

newtype DegreesOfFreedom = DegreesOfFreedom Int
instance showDegreesOfFreedom :: Show DegreesOfFreedom where
  show (DegreesOfFreedom df) = "DegreesOfFreedom " <> show df
derive instance newtypeDegreesOfFreedom :: Newtype DegreesOfFreedom _
derive newtype instance eqDegreesOfFreedom :: Eq DegreesOfFreedom
derive newtype instance ordDegreesOfFreedom :: Ord DegreesOfFreedom
instance boundedDegreesOfFreedom :: Bounded DegreesOfFreedom where
  bottom = DegreesOfFreedom 1
  top = DegreesOfFreedom 100
instance enumDegreesOfFreedom :: Enum DegreesOfFreedom where
  pred (DegreesOfFreedom df) = degreesOfFreedom (df - 1)
  succ (DegreesOfFreedom df) = degreesOfFreedom (df + 1)
instance boundedEnumDegreesOfFreedom :: BoundedEnum DegreesOfFreedom where
  fromEnum (DegreesOfFreedom df) = df - 1
  toEnum i = degreesOfFreedom (i + 1)
  cardinality = defaultCardinality

degreesOfFreedom :: Int -> Maybe DegreesOfFreedom
degreesOfFreedom i =
  let df = DegreesOfFreedom i
  in if df >= bottom && df <= top then Just df else Nothing

newtype CriticalValue = CriticalValue Number
derive newtype instance showCriticalValue :: Show CriticalValue
cvToNumber :: CriticalValue -> Number
cvToNumber (CriticalValue n) = n

lookup :: DegreesOfFreedom -> Alpha -> CriticalValue
lookup df a = CriticalValue $ (table `unsafeIndex` dfIndex df) `unsafeIndex` alphaIndex a
  where
    unsafeIndex :: forall a. Array a -> Int -> a
    unsafeIndex arr i = unsafePartial $ fromJust $ index arr i

    alphaIndex Alpha1   = 0
    alphaIndex Alpha05  = 1
    alphaIndex Alpha025 = 2
    alphaIndex Alpha01  = 3
    alphaIndex Alpha001 = 4

    dfIndex (DegreesOfFreedom df') = df' - 1

table :: Array (Array Number)
table = [[2.706,3.841,5.024,6.635,10.828],[4.605,5.991,7.378,9.21,13.816],[6.251,7.815,9.348,11.345,16.266],[7.779,9.488,11.143,13.277,18.467],[9.236,11.07,12.833,15.086,20.515],[10.645,12.592,14.449,16.812,22.458],[12.017,14.067,16.013,18.475,24.322],[13.362,15.507,17.535,20.09,26.125],[14.684,16.919,19.023,21.666,27.877],[15.987,18.307,20.483,23.209,29.588],[17.275,19.675,21.92,24.725,31.264],[18.549,21.026,23.337,26.217,32.91],[19.812,22.362,24.736,27.688,34.528],[21.064,23.685,26.119,29.141,36.123],[22.307,24.996,27.488,30.578,37.697],[23.542,26.296,28.845,32.0,39.252],[24.769,27.587,30.191,33.409,40.79],[25.989,28.869,31.526,34.805,42.312],[27.204,30.144,32.852,36.191,43.82],[28.412,31.41,34.17,37.566,45.315],[29.615,32.671,35.479,38.932,46.797],[30.813,33.924,36.781,40.289,48.268],[32.007,35.172,38.076,41.638,49.728],[33.196,36.415,39.364,42.98,51.179],[34.382,37.652,40.646,44.314,52.62],[35.563,38.885,41.923,45.642,54.052],[36.741,40.113,43.195,46.963,55.476],[37.916,41.337,44.461,48.278,56.892],[39.087,42.557,45.722,49.588,58.301],[40.256,43.773,46.979,50.892,59.703],[41.422,44.985,48.232,52.191,61.098],[42.585,46.194,49.48,53.486,62.487],[43.745,47.4,50.725,54.776,63.87],[44.903,48.602,51.966,56.061,65.247],[46.059,49.802,53.203,57.342,66.619],[47.212,50.998,54.437,58.619,67.985],[48.363,52.192,55.668,59.893,69.347],[49.513,53.384,56.896,61.162,70.703],[50.66,54.572,58.12,62.428,72.055],[51.805,55.758,59.342,63.691,73.402],[52.949,56.942,60.561,64.95,74.745],[54.09,58.124,61.777,66.206,76.084],[55.23,59.304,62.99,67.459,77.419],[56.369,60.481,64.201,68.71,78.75],[57.505,61.656,65.41,69.957,80.077],[58.641,62.83,66.617,71.201,81.4],[59.774,64.001,67.821,72.443,82.72],[60.907,65.171,69.023,73.683,84.037],[62.038,66.339,70.222,74.919,85.351],[63.167,67.505,71.42,76.154,86.661],[64.295,68.669,72.616,77.386,87.968],[65.422,69.832,73.81,78.616,89.272],[66.548,70.993,75.002,79.843,90.573],[67.673,72.153,76.192,81.069,91.872],[68.796,73.311,77.38,82.292,93.168],[69.919,74.468,78.567,83.513,94.461],[71.04,75.624,79.752,84.733,95.751],[72.16,76.778,80.936,85.95,97.039],[73.279,77.931,82.117,87.166,98.324],[74.397,79.082,83.298,88.379,99.607],[75.514,80.232,84.476,89.591,100.888],[76.63,81.381,85.654,90.802,102.166],[77.745,82.529,86.83,92.01,103.442],[78.86,83.675,88.004,93.217,104.716],[79.973,84.821,89.177,94.422,105.988],[81.085,85.965,90.349,95.626,107.258],[82.197,87.108,91.519,96.828,108.526],[83.308,88.25,92.689,98.028,109.791],[84.418,89.391,93.856,99.228,111.055],[85.527,90.531,95.023,100.425,112.317],[86.635,91.67,96.189,101.621,113.577],[87.743,92.808,97.353,102.816,114.835],[88.85,93.945,98.516,104.01,116.092],[89.956,95.081,99.678,105.202,117.346],[91.061,96.217,100.839,106.393,118.599],[92.166,97.351,101.999,107.583,119.85],[93.27,98.484,103.158,108.771,121.1],[94.374,99.617,104.316,109.958,122.348],[95.476,100.749,105.473,111.144,123.594],[96.578,101.879,106.629,112.329,124.839],[97.68,103.01,107.783,113.512,126.083],[98.78,104.139,108.937,114.695,127.324],[99.88,105.267,110.09,115.876,128.565],[100.98,106.395,111.242,117.057,129.804],[102.079,107.522,112.393,118.236,131.041],[103.177,108.648,113.544,119.414,132.277],[104.275,109.773,114.693,120.591,133.512],[105.372,110.898,115.841,121.767,134.746],[106.469,112.022,116.989,122.942,135.978],[107.565,113.145,118.136,124.116,137.208],[108.661,114.268,119.282,125.289,138.438],[109.756,115.39,120.427,126.462,139.666],[110.85,116.511,121.571,127.633,140.893],[111.944,117.632,122.715,128.803,142.119],[113.038,118.752,123.858,129.973,143.344],[114.131,119.871,125.0,131.141,144.567],[115.223,120.99,126.141,132.309,145.789],[116.315,122.108,127.282,133.476,147.01],[117.407,123.225,128.422,134.642,148.23],[118.498,124.342,129.561,135.807,149.449]]
