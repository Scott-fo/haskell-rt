module Main where

import Colour (writeColour)
import Ray (Ray (Ray), rayColour)
import Vec3 (Vec3 (Vec3), add, div, mul, sub)

main :: IO ()
main = do
  let aspectRatio = 16.0 / 9.0 :: Double
      imageWidth = 400 :: Double
      imageHeight = floor (imageWidth / aspectRatio) :: Int
      focalLength = 1.0 :: Double
      viewportHeight = 2.0 :: Double
      viewportWidth = viewportHeight * (imageWidth / fromIntegral imageHeight)

      cameraCenter = Vec3 0 0 0
      viewportU = Vec3 viewportWidth 0 0
      viewportV = Vec3 0 (-viewportHeight) 0

      pixelDeltaU = case viewportU `Vec3.div` imageWidth of
        Just v -> v
        Nothing -> error "DivideByZero"

      pixelDeltaV = case viewportV `Vec3.div` fromIntegral imageHeight of
        Just v -> v
        Nothing -> error "DivideByZero"

      viewportUpperLeft =
        cameraCenter
          `sub` Vec3 0 0 focalLength
          `sub` (viewportU `mul` 0.5)
          `sub` (viewportV `mul` 0.5)

      pixel00Loc = viewportUpperLeft `add` ((pixelDeltaU `add` pixelDeltaV) `mul` 0.5)

      header = ppmHeader (floor imageWidth) imageHeight
      body = renderImage (floor imageWidth) imageHeight pixel00Loc pixelDeltaU pixelDeltaV cameraCenter

  writeFile "image.ppm" (header ++ body)

renderImage :: Int -> Int -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> String
renderImage w h pixel00Loc pixelDeltaU pixelDeltaV cameraCenter =
  unlines
    [ writeColour (rayColour (Ray cameraCenter rayDirection))
      | j <- [0 .. h - 1],
        i <- [0 .. w - 1],
        let pixelCenter =
              pixel00Loc
                `add` (pixelDeltaU `mul` fromIntegral i)
                `add` (pixelDeltaV `mul` fromIntegral j)
            rayDirection = pixelCenter `sub` cameraCenter
    ]

ppmHeader :: Int -> Int -> String
ppmHeader w h =
  "P3\n" ++ show w ++ " " ++ show h ++ "\n255\n"
