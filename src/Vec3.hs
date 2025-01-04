module Vec3 where

import Utils (RayM, randomDouble, randomDoubleRange)

data Vec3 = Vec3 !Double !Double !Double
  deriving (Show, Eq)

type Point3 = Vec3

add :: Vec3 -> Vec3 -> Vec3
add (Vec3 ux uy uz) (Vec3 vx vy vz) = Vec3 (ux + vx) (uy + vy) (uz + vz)

sub :: Vec3 -> Vec3 -> Vec3
sub (Vec3 ux uy uz) (Vec3 vx vy vz) = Vec3 (ux - vx) (uy - vy) (uz - vz)

mul :: Vec3 -> Double -> Vec3
mul (Vec3 x' y' z') t = Vec3 (x' * t) (y' * t) (z' * t)

div :: Vec3 -> Double -> Maybe Vec3
div _ 0 = Nothing
div v t = Just $ mul v (1 / t)

negative :: Vec3 -> Vec3
negative (Vec3 x' y' z') = Vec3 (-x') (-y') (-z')

length :: Vec3 -> Double
length = sqrt . lengthSquared

lengthSquared :: Vec3 -> Double
lengthSquared (Vec3 x' y' z') = (x' * x') + (y' * y') + (z' * z')

dot :: Vec3 -> Vec3 -> Double
dot (Vec3 ux uy uz) (Vec3 vx vy vz) = (ux * vx) + (uy * vy) + (uz * vz)

cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3 ux uy uz) (Vec3 vx vy vz) =
  Vec3 (uy * vz - uz * vy) (uz * vx - ux * vz) (ux * vy - uy * vx)

unitVector :: Vec3 -> Maybe Vec3
unitVector v
  | vl == 0 = Nothing
  | otherwise = Vec3.div v vl
  where
    vl = Vec3.length v

nearZero :: Vec3 -> Bool
nearZero (Vec3 x' y' z') =
  x' < s && y' < s && z' < s
  where
    s = 1e-8

reflect :: Vec3 -> Vec3 -> Vec3
reflect v n =
  v `sub` mul n (2 * dot v n)

refract :: Vec3 -> Vec3 -> Double -> Vec3
refract uv n etaiOverEtat = rOutPerp `add` rOutParallel
  where
    cosTheta = min 1.0 $ dot (negative uv) n
    rOutPerp = mul (add uv (mul n cosTheta)) etaiOverEtat
    rOutParallel = mul n $ negate $ sqrt $ abs $ 1.0 - lengthSquared rOutPerp

randomVec3 :: RayM Vec3
randomVec3 = do
  x <- randomDouble
  y <- randomDouble
  Vec3 x y <$> randomDouble

randomVec3Range :: Double -> Double -> RayM Vec3
randomVec3Range min' max' = do
  x <- randomDoubleRange min' max'
  y <- randomDoubleRange min' max'
  z <- randomDoubleRange min' max'
  return $ Vec3 x y z

randomUnitVec3 :: RayM Vec3
randomUnitVec3 = do
  vec <- randomVec3Range (-1) 1
  maybe randomUnitVec3 return (unitVector vec)

randomOnHemisphere :: Vec3 -> RayM Vec3
randomOnHemisphere normal = do
  onUnitSphere <- randomUnitVec3
  return $
    if dot onUnitSphere normal > 0
      then onUnitSphere
      else negative onUnitSphere
