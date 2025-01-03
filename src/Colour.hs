module Colour
  ( Colour (Colour),
    writeColour,
    sumColours,
    addColours,
    mulColour,
    toRgb,
    mkColour,
  )
where

import Data.List (foldl')
import Vec3 (Vec3 (Vec3), add, mul)

newtype Colour = Colour Vec3
  deriving (Show, Eq)

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
sumColours = foldl' addColours (Colour (Vec3 0 0 0))

addColours :: Colour -> Colour -> Colour
addColours (Colour v1) (Colour v2) = Colour (Vec3.add v1 v2)

mulColour :: Double -> Colour -> Colour
mulColour s (Colour v) = Colour (mul v s)
