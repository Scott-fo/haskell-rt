module Interval where

data Interval = Interval
  { intervalMin :: Double,
    intervalMax :: Double
  }
  deriving (Show, Eq)

unsafeMkInterval :: Double -> Double -> Interval
unsafeMkInterval min' max'
  | min' <= max' = Interval min' max'
  | otherwise = error "Invalid interval bounds"

mkInterval :: Double -> Double -> Maybe Interval
mkInterval min' max'
  | min' <= max' = Just $ Interval min' max'
  | otherwise = Nothing

size :: Interval -> Double
size (Interval min' max') = max' - min'

contains :: Interval -> Double -> Bool
contains (Interval min' max') x = min' <= x && x <= max'

surrounds :: Interval -> Double -> Bool
surrounds (Interval min' max') x = min' < x && x < max'

clamp :: Interval -> Double -> Double
clamp (Interval min' max') = max min' . min max'
