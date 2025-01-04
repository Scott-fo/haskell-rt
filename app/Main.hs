module Main where

import Camera (render)
import Colour (Colour (Colour), mulColours, randomColour, randomColourRange)
import Control.Exception (IOException, try)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Hittable (Dielectric (Dielectric), Lambertian (Lambertian), MaterialWrapper (MaterialWrapper), Metal (Metal))
import HittableList (HittableList, fromList)
import Sphere (Sphere (Sphere), center, material, radius)
import System.IO (hPutStrLn, stderr)
import System.Random (mkStdGen)
import Utils (RayM, randomDouble, randomDoubleRange, runRayM)
import Vec3 (Vec3 (Vec3), length, sub)

main :: IO ()
main = do
  let initialGen = mkStdGen 42
      (world, _) = runRayM buildWorld initialGen

  image <- render world

  result <- try $ TIO.writeFile "image.ppm" (T.pack image)
  case result of
    Left e -> hPutStrLn stderr $ "Error writing to file: " ++ show (e :: IOException)
    Right _ -> putStrLn "Image successfully written to image.ppm"

buildWorld :: RayM HittableList
buildWorld = do
  let groundMaterial = MaterialWrapper $ Lambertian $ Colour $ Vec3 0.5 0.5 0.5
      groundSphere =
        Sphere
          { center = Vec3 0.0 (-1000) 0,
            radius = 1000.0,
            material = groundMaterial
          }

  randomSpheres <-
    sequence
      [ do
          rx <- randomDouble
          rz <- randomDouble
          let center' = Vec3 (a + 0.9 * rx) 0.2 (b + 0.9 * rz)
          let comparePoint = Vec3 4 0.2 0

          if Vec3.length (Vec3.sub center' comparePoint) > 0.9
            then do
              chooseMat <- randomDouble
              sphereMaterial <-
                if chooseMat < 0.8
                  then do
                    c1 <- randomColour
                    MaterialWrapper . Lambertian . mulColours c1 <$> randomColour
                  else
                    if chooseMat < 0.95
                      then do
                        albedo <- randomColourRange 0.5 1
                        fuzz <- randomDoubleRange 0 0.5
                        return $ MaterialWrapper $ Metal albedo fuzz
                      else
                        return $ MaterialWrapper $ Dielectric 1.5

              return $ Just $ Sphere center' 0.2 sphereMaterial
            else return Nothing
        | a <- [-11 .. 11],
          b <- [-11 .. 11]
      ]

  let material1 = MaterialWrapper $ Dielectric 1.5
      sphere1 = Sphere (Vec3 0 1 0) 1.0 material1

      material2 = MaterialWrapper $ Lambertian $ Colour $ Vec3 0.4 0.2 0.1
      sphere2 = Sphere (Vec3 (-4) 1 0) 1.0 material2

      material3 = MaterialWrapper $ Metal (Colour $ Vec3 0.7 0.6 0.5) 0.0
      sphere3 = Sphere (Vec3 4 1 0) 1.0 material3

  return $ fromList $ groundSphere : sphere1 : sphere2 : sphere3 : catMaybes randomSpheres
