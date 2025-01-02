module Colour where

import Vec3 (Vec3 (Vec3))

type Colour = Vec3

writeColour :: Colour -> String
writeColour (Vec3 r g b) =
  let rbyte = floor (255.999 * r) :: Int
      gbyte = floor (255.999 * g) :: Int
      bbyte = floor (255.999 * b) :: Int
   in unwords [show rbyte, show gbyte, show bbyte]