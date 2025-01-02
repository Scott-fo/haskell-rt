module Colour
  ( Colour (Colour),
    writeColour,
    toRgb,
    mkColour,
  )
where

import Vec3 (Vec3 (Vec3))

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
