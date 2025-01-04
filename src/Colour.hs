{-# LANGUAGE DeriveGeneric #-}

module Colour
  ( Colour (Colour),
    writeColour,
    sumColours,
    addColours,
    mulColour,
    mulColours,
    toRgb,
    mkColour,
    randomColour,
    randomColourRange,
    white,
    black,
    blue,
  )
where

import Control.Parallel.Strategies (NFData)
import Data.List (foldl')
import GHC.Generics (Generic)
import Utils (RayM)
import Vec3 (Vec3 (Vec3), add, mul, randomVec3, randomVec3Range)

newtype Colour = Colour Vec3
  deriving (Show, Eq, Generic)

instance NFData Colour

mkColour :: Double -> Double -> Double -> Colour
mkColour r g b = Colour $ Vec3 (clamp r) (clamp g) (clamp b)
  where
    clamp :: Double -> Double
    clamp = max 0 . min 1

toRgb :: Colour -> (Int, Int, Int)
toRgb (Colour (Vec3 r g b)) = (channelToByte r, channelToByte g, channelToByte b)
  where
    channelToByte :: Double -> Int
    channelToByte = floor . (* 255.999) . clamp
    clamp = max 0 . min 1

writeColour :: Colour -> String
writeColour col =
  let (r, g, b) = toRgb col
   in unwords [show r, show g, show b]

sumColours :: [Colour] -> Colour
sumColours = foldl' addColours black

addColours :: Colour -> Colour -> Colour
addColours (Colour v1) (Colour v2) = Colour (Vec3.add v1 v2)

mulColour :: Colour -> Double -> Colour
mulColour (Colour v) s = Colour (mul v s)

mulColours :: Colour -> Colour -> Colour
mulColours (Colour (Vec3 x1 y1 z1)) (Colour (Vec3 x2 y2 z2)) = mkColour (x1 * x2) (y1 * y2) (z1 * z2)

randomColour :: RayM Colour
randomColour = do Colour <$> randomVec3

randomColourRange :: Double -> Double -> RayM Colour
randomColourRange min' max' = do Colour <$> randomVec3Range min' max'

black :: Colour
black = mkColour 0 0 0

white :: Colour
white = mkColour 1 1 1

blue :: Colour
blue = mkColour 0.5 0.7 1.0
