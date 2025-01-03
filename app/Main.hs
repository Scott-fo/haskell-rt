module Main where

import Camera (render)
import Colour (Colour (Colour))
import Control.Exception (IOException, try)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Hittable (Lambertian (Lambertian), MaterialWrapper (MaterialWrapper), Metal (Metal))
import HittableList (fromList)
import Sphere (Sphere (Sphere), center, material, radius)
import System.IO (hPutStrLn, stderr)
import Vec3 (Vec3 (Vec3))

main :: IO ()
main = do
  let groundMaterial = MaterialWrapper $ Lambertian $ Colour $ Vec3 0.8 0.8 0.0
      centerMaterial = MaterialWrapper $ Lambertian $ Colour $ Vec3 0.1 0.2 0.5
      leftMaterial = MaterialWrapper $ Metal $ Colour $ Vec3 0.8 0.8 0.8
      rightMaterial = MaterialWrapper $ Metal $ Colour $ Vec3 0.8 0.6 0.2

      groundSphere =
        Sphere
          { center = Vec3 0.0 (-100.5) (-1.0),
            radius = 100.0,
            material = groundMaterial
          }
      centerSphere =
        Sphere
          { center = Vec3 0.0 0.0 (-1.2),
            radius = 0.5,
            material = centerMaterial
          }
      leftSphere =
        Sphere
          { center = Vec3 (-1.0) 0.0 (-1.0),
            radius = 0.5,
            material = leftMaterial
          }
      rightSphere =
        Sphere
          { center = Vec3 1.0 0.0 (-1.0),
            radius = 0.5,
            material = rightMaterial
          }

      world = fromList [groundSphere, centerSphere, leftSphere, rightSphere]

  image <- render world

  result <- try $ TIO.writeFile "image.ppm" (T.pack image)
  case result of
    Left e -> hPutStrLn stderr $ "Error writing to file: " ++ show (e :: IOException)
    Right _ -> putStrLn "Image successfully written to image.ppm"
