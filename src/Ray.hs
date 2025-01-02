module Ray where

import Vec3 (Point3, Vec3, add, mul)

data Ray = Ray
  { origin :: Point3,
    direction :: Vec3
  }

at :: Ray -> Double -> Vec3
at (Ray o d) t = add o (mul d t)
