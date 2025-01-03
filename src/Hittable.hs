module Hittable
  ( HitRecord (..),
    Hittable (..),
    mkHitRecord,
    setFaceNormal,
  )
where

import Interval (Interval)
import Ray (Ray (direction))
import Vec3 (Point3, Vec3, dot, negative)

-- Records the details of a ray-object intersection
data HitRecord = HitRecord
  { -- Point of intersection
    hitPoint :: !Point3,
    -- Surface normal at intersection
    normal :: !Vec3,
    -- Distance along ray to intersection
    hitT :: !Double,
    -- Whether ray hit the front face
    frontFace :: !Bool
  }
  deriving (Show)

-- Smart constructor for HitRecord that automatically sets face normal
mkHitRecord ::
  -- Intersection point
  Point3 ->
  -- Outward normal (before orientation)
  Vec3 ->
  -- Ray parameter t
  Double ->
  -- The ray that caused the hit
  Ray ->
  HitRecord
mkHitRecord p outwardNormal t ray =
  let isFrontFace = direction ray `dot` outwardNormal < 0
      finalNormal =
        if isFrontFace
          then outwardNormal
          else negative outwardNormal
   in HitRecord
        { hitPoint = p,
          normal = finalNormal,
          hitT = t,
          frontFace = isFrontFace
        }

-- Class for objects that can be hit by a ray
class Hittable a where
  -- Determine if and where a ray intersects an object
  hit ::
    -- The object to test
    a ->
    -- The ray to test with
    Ray ->
    Interval ->
    -- Hit information if intersection occurred
    Maybe HitRecord

setFaceNormal :: Ray -> Vec3 -> HitRecord -> HitRecord
setFaceNormal ray outwardNormal record =
  let isFrontFace = direction ray `dot` outwardNormal < 0
      correctedNormal =
        if isFrontFace
          then outwardNormal
          else negative outwardNormal
   in record
        { normal = correctedNormal,
          frontFace = isFrontFace
        }
