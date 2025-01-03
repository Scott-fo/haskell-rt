module Sphere (Sphere (..)) where

import Control.Monad (guard)
import Data.List (find)
import Hittable (HitRecord (HitRecord, frontFace, hitPoint, hitT, normal), Hittable (hit), setFaceNormal)
import Interval (surrounds)
import Ray (Ray (direction, origin), at)
import Vec3 (Point3, dot, lengthSquared, mul, sub)

data Sphere = Sphere
  { center :: Point3,
    radius :: Double
  }
  deriving (Show)

instance Hittable Sphere where
  hit sphere ray interval = do
    let oc = center sphere `sub` origin ray
        a = lengthSquared (direction ray)
        h = dot (direction ray) oc
        c = lengthSquared oc - radius sphere ^ (2 :: Int)
        discriminant = h * h - a * c

    guard (discriminant >= 0)
    let sqrtd = sqrt discriminant
        roots = [(h - sqrtd) / a, (h + sqrtd) / a]
        validRoot = find (surrounds interval) roots

    root <- validRoot
    let hPoint = at ray root
        outwardNormal = (hPoint `sub` center sphere) `mul` (1 / radius sphere)
        record = HitRecord {hitPoint = hPoint, normal = outwardNormal, hitT = root, frontFace = False}

    return $ setFaceNormal ray outwardNormal record
