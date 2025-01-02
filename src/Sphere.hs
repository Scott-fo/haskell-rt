module Sphere where

import Hittable (HitRecord (HitRecord), Hittable (hit), setFaceNormal)
import Ray (Ray (Ray), at)
import Vec3 (Point3, dot, lengthSquared, mul, sub)

data Sphere = Sphere
  { center :: Point3,
    radius :: Double
  }
  deriving (Show)

instance Hittable Sphere where
  hit (Sphere center' radius') (Ray origin direction) tmin tmax =
    let oc = origin `sub` center'
        a = lengthSquared direction
        h = dot direction oc
        c = lengthSquared oc - radius' * radius'
        discriminant = h * h - a * c
     in if discriminant < 0
          then Nothing
          else
            let sqrtd = sqrt discriminant
                root1 = (h - sqrtd) / a
                root2 = (h + sqrtd) / a
                root = if root1 > tmin && root1 < tmax then root1 else root2
             in if root <= tmin || root >= tmax
                  then Nothing
                  else
                    let p = at (Ray origin direction) root
                        outwardNormal = (p `sub` center') `mul` (1 / radius')
                        record = HitRecord p outwardNormal root False
                     in Just $ setFaceNormal (Ray origin direction) outwardNormal record
