module Hittable where

import Ray (Ray (direction))
import Vec3 (Point3, Vec3, dot, negative)

data HitRecord = HitRecord
  { p :: Point3,
    normal :: Vec3,
    t :: Double,
    frontFace :: Bool
  }
  deriving (Show)

class Hittable a where
  hit :: a -> Ray -> Double -> Double -> Maybe HitRecord

setFaceNormal :: Ray -> Vec3 -> HitRecord -> HitRecord
setFaceNormal ray outwardNormal record =
  let isFrontFace = direction ray `dot` outwardNormal < 0
      correctedNormal =
        if isFrontFace
          then outwardNormal
          else negative outwardNormal
   in record {normal = correctedNormal, frontFace = isFrontFace}
