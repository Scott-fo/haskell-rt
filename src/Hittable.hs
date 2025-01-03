{-# LANGUAGE ExistentialQuantification #-}

module Hittable
  ( HitRecord (..),
    Hittable (..),
    MaterialWrapper (..),
    ScatterResult (..),
    Material (..),
    Lambertian (..),
    Metal (..),
    mkHitRecord,
    setFaceNormal,
  )
where

import Colour (Colour)
import Interval (Interval)
import Ray (Ray (Ray, direction))
import Utils (RayM)
import Vec3 (Point3, Vec3, add, dot, nearZero, negative, randomUnitVec3, reflect)

data ScatterResult = ScatterResult
  { scattered :: Ray,
    attenuation :: Colour
  }

class Material a where
  -- Attempt to scatter a ray.
  -- If absorbed, returns nothing. Else, returns the ScatterResult
  scatter :: a -> Ray -> HitRecord -> RayM (Maybe ScatterResult)

data MaterialWrapper = forall a. (Material a) => MaterialWrapper a

instance Show MaterialWrapper where
  show _ = "Material. Placeholder."

newtype Lambertian = Lambertian Colour
  deriving (Show)

instance Material Lambertian where
  scatter (Lambertian albedo) _ rec = do
    randomUnit <- randomUnitVec3
    let scatterDirection = Vec3.add (normal rec) randomUnit
        scatterDirection' = if nearZero scatterDirection then normal rec else scatterDirection
        scattered' = Ray (hitPoint rec) scatterDirection'
    return $ Just $ ScatterResult scattered' albedo

newtype Metal = Metal Colour
  deriving (Show)

instance Material Metal where
  scatter (Metal albedo) r_in rec = do
    let reflected = reflect (direction r_in) (normal rec)
        scattered' = Ray (hitPoint rec) reflected
    return $ Just $ ScatterResult scattered' albedo

-- Records the details of a ray-object intersection
data HitRecord = HitRecord
  { -- Point of intersection
    hitPoint :: !Point3,
    -- Surface normal at intersection
    normal :: !Vec3,
    -- Material
    mat :: MaterialWrapper,
    -- Distance along ray to intersection
    hitT :: !Double,
    -- Whether ray hit the front face
    frontFace :: !Bool
  }

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
  MaterialWrapper ->
  HitRecord
mkHitRecord p outwardNormal t ray material =
  let isFrontFace = direction ray `dot` outwardNormal < 0
      finalNormal =
        if isFrontFace
          then outwardNormal
          else negative outwardNormal
   in HitRecord
        { hitPoint = p,
          normal = finalNormal,
          hitT = t,
          frontFace = isFrontFace,
          mat = material
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

setFaceNormal :: HitRecord -> Ray -> Vec3 -> HitRecord
setFaceNormal record ray outwardNormal =
  let isFrontFace = direction ray `dot` outwardNormal < 0
      correctedNormal =
        if isFrontFace
          then outwardNormal
          else negative outwardNormal
   in record
        { normal = correctedNormal,
          frontFace = isFrontFace
        }
