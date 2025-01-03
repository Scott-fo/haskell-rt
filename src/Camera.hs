{-# LANGUAGE RecordWildCards #-}

module Camera
  ( Camera (Camera),
    aspectRatio,
    imageWidth,
    initialize,
    render,
  )
where

import Colour (Colour (Colour), mkColour, writeColour)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Hittable (HitRecord (HitRecord, normal), Hittable (hit))
import HittableList (HittableList)
import Interval (unsafeMkInterval)
import Ray (Ray (Ray, direction))
import Vec3 (Point3, Vec3 (Vec3), add, mul, sub, unitVector)

data Camera = Camera
  { aspectRatio :: Double,
    imageWidth :: Int,
    imageHeight :: Int,
    center :: Point3,
    pixel00Loc :: Point3,
    pixelDeltaU :: Vec3,
    pixelDeltaV :: Vec3
  }
  deriving (Show)

initialize :: Camera
initialize =
  let aspectRatio = 16.0 / 9.0 :: Double
      imageWidth = 400 :: Int
      imageHeight = floor (fromIntegral imageWidth / aspectRatio)

      focalLength = 1.0 :: Double
      viewportHeight = 2.0 :: Double
      viewportWidth = viewportHeight * (fromIntegral imageWidth / fromIntegral imageHeight)

      center = Vec3 0 0 0 :: Point3
      viewportU = Vec3 viewportWidth 0 0
      viewportV = Vec3 0 (-viewportHeight) 0

      pixelDeltaU = mul viewportU (1 / fromIntegral imageWidth)
      pixelDeltaV = mul viewportV (1 / fromIntegral imageHeight)

      viewportUpperLeft =
        foldl'
          sub
          center
          [ Vec3 0 0 focalLength,
            mul viewportU 0.5,
            mul viewportV 0.5
          ]

      pixel00Loc = viewportUpperLeft `Vec3.add` ((pixelDeltaU `Vec3.add` pixelDeltaV) `mul` 0.5)
   in Camera {..}

render :: HittableList -> String
render world = header ++ body
  where
    Camera {..} = initialize
    header = ppmHeader imageWidth imageHeight
    body =
      unlines
        [ writeColour (rayColour ray world)
          | j <- [0 .. imageHeight - 1],
            i <- [0 .. imageWidth - 1],
            let pixelCenter =
                  pixel00Loc
                    `Vec3.add` (pixelDeltaU `mul` fromIntegral i)
                    `Vec3.add` (pixelDeltaV `mul` fromIntegral j),
            let rayDirection = pixelCenter `sub` center,
            let ray = Ray center rayDirection
        ]

rayColour :: Ray -> HittableList -> Colour
rayColour ray world = fromMaybe (backgroundColour ray) $ do
  let i = unsafeMkInterval 0 infinity
  rec <- hit world ray i
  return $ sphereColour rec

sphereColour :: HitRecord -> Colour
sphereColour HitRecord {..} = Colour $ mul (Vec3.add normal (Vec3 1 1 1)) 0.5

backgroundColour :: Ray -> Colour
backgroundColour Ray {..} = fromMaybe (mkColour 0 0 0) $ do
  Vec3 _ y _ <- unitVector direction
  let t = 0.5 * (y + 1.0)
      white = Vec3 1.0 1.0 1.0
      blue = Vec3 0.5 0.7 1.0
  return $ Colour $ Vec3.add (mul white (1.0 - t)) (mul blue t)

ppmHeader :: Int -> Int -> String
ppmHeader w h =
  "P3\n" ++ show w ++ " " ++ show h ++ "\n255\n"

infinity :: Double
infinity = 1 / 0
