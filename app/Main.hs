{-# LANGUAGE RecordWildCards #-}

module Main where

import Colour (Colour (Colour), mkColour, writeColour)
import Control.Exception (IOException, try)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Hittable (HitRecord (HitRecord, normal), Hittable (hit))
import HittableList (HittableList, fromList)
import Ray (Ray (Ray, direction))
import Sphere (Sphere (Sphere))
import System.IO (hPutStrLn, stderr)
import Vec3 (Vec3 (Vec3), add, mul, sub, unitVector)

data RenderConfig = RenderConfig
  { imageWidth :: Int,
    imageHeight :: Int,
    pixel00Loc :: Vec3,
    pixelDeltaU :: Vec3,
    pixelDeltaV :: Vec3,
    cameraCenter :: Vec3,
    world :: HittableList
  }

instance Show RenderConfig where
  show config =
    unlines
      [ "RenderConfig:",
        "  imageWidth: " ++ show (imageWidth config),
        "  imageHeight: " ++ show (imageHeight config),
        "  pixel00Loc: " ++ show (pixel00Loc config),
        "  pixelDeltaU: " ++ show (pixelDeltaU config),
        "  pixelDeltaV: " ++ show (pixelDeltaV config),
        "  cameraCenter: " ++ show (cameraCenter config),
        "  world: <HittableList>"
      ]

defaultConfig :: RenderConfig
defaultConfig =
  let aspectRatio = 16.0 / 9.0 :: Double
      imageWidth = 400 :: Int
      imageHeight = floor (fromIntegral imageWidth / aspectRatio)

      sphere1 = Sphere (Vec3 0 0 (-1)) 0.5
      sphere2 = Sphere (Vec3 0 (-100.5) (-1)) 100

      world = fromList [sphere1, sphere2]

      focalLength = 1.0 :: Double
      viewportHeight = 2.0 :: Double
      viewportWidth = viewportHeight * (fromIntegral imageWidth / fromIntegral imageHeight)

      cameraCenter = Vec3 0 0 0
      viewportU = Vec3 viewportWidth 0 0
      viewportV = Vec3 0 (-viewportHeight) 0

      pixelDeltaU = mul viewportU (1 / fromIntegral imageWidth)
      pixelDeltaV = mul viewportV (1 / fromIntegral imageHeight)

      viewportUpperLeft =
        foldl'
          sub
          cameraCenter
          [ Vec3 0 0 focalLength,
            mul viewportU 0.5,
            mul viewportV 0.5
          ]

      pixel00Loc = viewportUpperLeft `Vec3.add` ((pixelDeltaU `Vec3.add` pixelDeltaV) `mul` 0.5)
   in RenderConfig {..}

main :: IO ()
main = do
  let config = defaultConfig
      header = ppmHeader (imageWidth config) (imageHeight config)
      body = renderImage config

  result <- try $ TIO.writeFile "image.ppm" (T.pack $ header ++ body)
  case result of
    Left e -> hPutStrLn stderr $ "Error writing to file: " ++ show (e :: IOException)
    Right _ -> putStrLn "Image successfully written to image.ppm"

renderImage :: RenderConfig -> String
renderImage RenderConfig {..} =
  unlines
    [ writeColour (rayColour ray world)
      | j <- [0 .. imageHeight - 1],
        i <- [0 .. imageWidth - 1],
        let pixelCenter =
              pixel00Loc
                `Vec3.add` (pixelDeltaU `mul` fromIntegral i)
                `Vec3.add` (pixelDeltaV `mul` fromIntegral j),
        let rayDirection = pixelCenter `sub` cameraCenter,
        let ray = Ray cameraCenter rayDirection
    ]

ppmHeader :: Int -> Int -> String
ppmHeader w h =
  "P3\n" ++ show w ++ " " ++ show h ++ "\n255\n"

rayColour :: Ray -> HittableList -> Colour
rayColour ray world = fromMaybe (backgroundColour ray) $ do
  rec <- hit world ray 0 infinity
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

infinity :: Double
infinity = 1 / 0
