module Ray where

import Colour (Colour)
import Vec3 (Point3, Vec3 (Vec3), add, mul, unitVector)

data Ray = Ray
  { origin :: Point3,
    direction :: Vec3
  }

at :: Ray -> Double -> Vec3
at (Ray o d) t = add o (mul d t)

rayColour :: Ray -> Colour
rayColour (Ray _ d) =
  case Vec3.unitVector d of
    Just (Vec3 _ y _) ->
      let a = 0.5 * (y + 1.0)
          white = Vec3 1.0 1.0 1.0
          blue = Vec3 0.5 0.7 1.0
       in Vec3.add (Vec3.mul white (1.0 - a)) (Vec3.mul blue a)
    Nothing -> Vec3 0 0 0
