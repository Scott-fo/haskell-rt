module Main where

import Colour (writeColour)
import Ray (Ray (Ray), rayColour)
import Vec3 (Vec3 (Vec3), add, div, mul, sub)

main :: IO ()
main = do
  let aspectRatio = 16.0 / 9.0 :: Double
      iw = 400 :: Int
      ih = floor (fromIntegral iw / aspectRatio) :: Int
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
            cameraCenter = cc
          }

      header = ppmHeader iw ih
      body = renderImage renderConfig

  writeFile "image.ppm" (header ++ body)

data RenderConfig = RenderConfig
  { imageWidth :: Int,
    imageHeight :: Int,
    pixel00Loc :: Vec3,
    pixelDeltaU :: Vec3,
    pixelDeltaV :: Vec3,
    cameraCenter :: Vec3
  }

renderImage :: RenderConfig -> String
renderImage config =
  let w = imageWidth config
      h = imageHeight config
      pixel00 = pixel00Loc config
      deltaU = pixelDeltaU config
      deltaV = pixelDeltaV config
      camera = cameraCenter config
   in unlines
        [ writeColour (rayColour (Ray camera rayDirection))
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
