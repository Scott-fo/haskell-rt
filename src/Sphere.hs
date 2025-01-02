module Sphere where

import Control.Monad (guard)
import Data.List (find)
import Hittable (HitRecord (HitRecord, frontFace, normal, p, t), Hittable (hit), setFaceNormal)
import Ray (Ray (direction, origin), at)
import Vec3 (Point3, dot, lengthSquared, mul, sub)

data Sphere = Sphere
  { center :: Point3,
    radius :: Double
  }
  deriving (Show)

instance Hittable Sphere where
  hit sphere ray tmin tmax = do
    let oc = center sphere `sub` origin ray
        a = lengthSquared (direction ray)
        h = dot (direction ray) oc
        c = lengthSquared oc - radius sphere ^ (2 :: Int)
        discriminant = h * h - a * c

    guard (discriminant >= 0)
    let sqrtd = sqrt discriminant
        roots = [(h - sqrtd) / a, (h + sqrtd) / a]
        validRoot = find (\r -> r > tmin && r < tmax) roots

    root <- validRoot
    let hitPoint = at ray root
        outwardNormal = (hitPoint `sub` center sphere) `mul` (1 / radius sphere)
        record = HitRecord {p = hitPoint, normal = outwardNormal, t = root, frontFace = False}

    return $ setFaceNormal ray outwardNormal record
