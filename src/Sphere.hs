module Sphere (Sphere (..)) where

import Control.Monad (guard)
import Data.List (find)
import Hittable (HitRecord (HitRecord, frontFace, hitPoint, hitT, mat, normal), Hittable (hit), MaterialWrapper, setFaceNormal)
import Interval (surrounds)
import Ray (Ray (direction, origin), at)
import Vec3 (Point3, dot, lengthSquared, mul, sub)

data Sphere = Sphere
  { center :: Point3,
    radius :: Double,
    material :: MaterialWrapper
  }
  deriving (Show)

instance Hittable Sphere where
  hit (Sphere center' radius' material') ray interval = do
    let oc = center' `sub` origin ray
        a = lengthSquared (direction ray)
        h = dot (direction ray) oc
        c = lengthSquared oc - radius' ^ (2 :: Int)
        discriminant = h * h - a * c

    guard (discriminant >= 0)
    let sqrtd = sqrt discriminant
        roots = [(h - sqrtd) / a, (h + sqrtd) / a]
        validRoot = find (surrounds interval) roots

    root <- validRoot
    let hPoint = at ray root
        outwardNormal = (hPoint `sub` center') `mul` (1 / radius')
        record = HitRecord {hitPoint = hPoint, normal = outwardNormal, hitT = root, frontFace = False, mat = material'}

    return $ setFaceNormal record ray outwardNormal
