module Ray where

import Colour (Colour)
import Vec3 (Point3, Vec3 (Vec3), add, dot, mul, sub, unitVector)

data Ray = Ray
  { origin :: Point3,
    direction :: Vec3
  }

at :: Ray -> Double -> Vec3
at (Ray o d) t = add o (mul d t)

rayColour :: Ray -> Colour
rayColour r =
  case unitVector (direction r) of
    Just (Vec3 _ y _) ->
      if hitSphere (Vec3 0 0 (-1)) 0.5 r
        then Vec3 1 0 0
        else
          let a = 0.5 * (y + 1.0)
              white = Vec3 1.0 1.0 1.0
              blue = Vec3 0.5 0.7 1.0
           in (white `mul` (1.0 - a)) `add` (blue `mul` a)
    Nothing -> Vec3 0 0 0

hitSphere :: Point3 -> Double -> Ray -> Bool
hitSphere center radius ray =
  discriminant center radius ray >= 0

discriminant :: Point3 -> Double -> Ray -> Double
discriminant center radius (Ray origin' direction') =
  let oc = center `sub` origin'
      a = dot direction' direction'
      b = -(2.0 * dot direction' oc)
      c = dot oc oc - (radius * radius)
   in b * b - 4 * a * c
