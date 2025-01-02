module Ray where

import Colour (Colour)
import Vec3 (Point3, Vec3 (Vec3), add, dot, lengthSquared, mul, sub, unitVector)

data Ray = Ray
  { origin :: Point3,
    direction :: Vec3
  }

at :: Ray -> Double -> Point3
at (Ray o d) t = add o (mul d t)

rayColour :: Ray -> Colour
rayColour r =
  case hitSphere (Vec3 0 0 (-1)) 0.5 r of
    Just t -> sphereColour r t
    Nothing -> backgroundColour r

sphereColour :: Ray -> Double -> Vec3
sphereColour r t =
  let n = normalize (at r t `sub` Vec3 0 0 (-1))
   in (n `mul` 0.5) `add` Vec3 0.5 0.5 0.5

backgroundColour :: Ray -> Vec3
backgroundColour r =
  case unitVector (direction r) of
    Just (Vec3 _ y _) ->
      let t = 0.5 * (y + 1.0)
          white = Vec3 1.0 1.0 1.0
          blue = Vec3 0.5 0.7 1.0
       in (white `mul` (1.0 - t)) `add` (blue `mul` t)
    Nothing -> Vec3 0 0 0

hitSphere :: Point3 -> Double -> Ray -> Maybe Double
hitSphere center radius (Ray origin' direction') =
  let oc = center `sub` origin'
      a = lengthSquared direction'
      h = dot direction' oc
      c = lengthSquared oc - (radius * radius)
      discriminant = h * h - a * c
   in if discriminant < 0
        then Nothing
        else Just ((h - sqrt discriminant) / a)

normalize :: Vec3 -> Vec3
normalize v = case unitVector v of
  Just u -> u
  Nothing -> Vec3 0 0 0
