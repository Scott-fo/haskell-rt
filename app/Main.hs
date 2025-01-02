module Main where

import Colour (Colour, writeColour)
import Hittable (HitRecord (normal), Hittable (hit))
import HittableList (HittableList, addHittable, emptyHittableList)
import Ray (Ray (Ray, direction))
import Sphere (Sphere (Sphere))
import Vec3 (Vec3 (Vec3), add, div, mul, sub, unitVector)

data RenderConfig = RenderConfig
  { imageWidth :: Int,
    imageHeight :: Int,
    pixel00Loc :: Vec3,
    pixelDeltaU :: Vec3,
    pixelDeltaV :: Vec3,
    cameraCenter :: Vec3,
    world :: HittableList
  }

main :: IO ()
main = do
  let aspectRatio = 16.0 / 9.0 :: Double
      iw = 400 :: Int
      ih = floor (fromIntegral iw / aspectRatio) :: Int

      sphere1 = Sphere (Vec3 0 0 (-1)) 0.5
      sphere2 = Sphere (Vec3 0 (-100.5) (-1)) 100

      w = addHittable (addHittable emptyHittableList sphere1) sphere2

      focalLength = 1.0 :: Double
      viewportHeight = 2.0 :: Double
      viewportWidth = viewportHeight * (fromIntegral iw / fromIntegral ih)

      cc = Vec3 0 0 0
      viewportU = Vec3 viewportWidth 0 0
      viewportV = Vec3 0 (-viewportHeight) 0

      pDU = case viewportU `Vec3.div` fromIntegral iw of
        Just v -> v
        Nothing -> error "DivideByZero"

      pDV = case viewportV `Vec3.div` fromIntegral ih of
        Just v -> v
        Nothing -> error "DivideByZero"

      viewportUpperLeft =
        cc
          `sub` Vec3 0 0 focalLength
          `sub` (viewportU `mul` 0.5)
          `sub` (viewportV `mul` 0.5)

      pLoc = viewportUpperLeft `add` ((pDU `add` pDV) `mul` 0.5)

      renderConfig =
        RenderConfig
          { imageWidth = iw,
            imageHeight = ih,
            pixel00Loc = pLoc,
            pixelDeltaU = pDU,
            pixelDeltaV = pDV,
            cameraCenter = cc,
            world = w
          }

      header = ppmHeader iw ih
      body = renderImage renderConfig

  writeFile "image.ppm" (header ++ body)

renderImage :: RenderConfig -> String
renderImage config =
  let w = imageWidth config
      h = imageHeight config
      pixel00 = pixel00Loc config
      deltaU = pixelDeltaU config
      deltaV = pixelDeltaV config
      camera = cameraCenter config
      hl = world config
   in unlines
        [ writeColour (rayColour (Ray camera rayDirection) hl)
          | j <- [0 .. h - 1],
            i <- [0 .. w - 1],
            let pixelCenter =
                  pixel00
                    `add` (deltaU `mul` fromIntegral i)
                    `add` (deltaV `mul` fromIntegral j)
                rayDirection = pixelCenter `sub` camera
        ]

ppmHeader :: Int -> Int -> String
ppmHeader w h =
  "P3\n" ++ show w ++ " " ++ show h ++ "\n255\n"

rayColour :: Ray -> HittableList -> Colour
rayColour r world' =
  case hit world' r 0 infinity of
    Just rec -> sphereColour rec
    Nothing -> backgroundColour r

sphereColour :: HitRecord -> Vec3
sphereColour rec = mul (normal rec `add` Vec3 1 1 1) 0.5

backgroundColour :: Ray -> Vec3
backgroundColour r =
  case unitVector (direction r) of
    Just (Vec3 _ y _) ->
      let t = 0.5 * (y + 1.0)
          white = Vec3 1.0 1.0 1.0
          blue = Vec3 0.5 0.7 1.0
       in (white `mul` (1.0 - t)) `add` (blue `mul` t)
    Nothing -> Vec3 0 0 0

infinity :: Double
infinity = 1 / 0
