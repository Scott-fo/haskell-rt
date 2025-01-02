module Vec3 where

data Vec3 = Vec3
  { x :: Double,
    y :: Double,
    z :: Double
  }
  deriving (Show, Eq)

add :: Vec3 -> Vec3 -> Vec3
add (Vec3 ux uy uz) (Vec3 vx vy vz) = Vec3 (ux + vx) (uy + vy) (uz + vz)

sub :: Vec3 -> Vec3 -> Vec3
sub (Vec3 ux uy uz) (Vec3 vx vy vz) = Vec3 (ux - vx) (uy - vy) (uz - vz)

mul :: Vec3 -> Double -> Vec3
mul (Vec3 x' y' z') t = Vec3 (x' * t) (y' * t) (z' * t)

div :: Vec3 -> Double -> Vec3
div _ 0 = error "DivideByZero"
div (Vec3 x' y' z') t = Vec3 (x' / t) (y' / t) (z' / t)

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

-- Not handling zero length case here yet
unitVector :: Vec3 -> Vec3
unitVector v = Vec3.div v (Vec3.length v)
