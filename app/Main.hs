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

generateSphereMaterial :: RayM MaterialWrapper
generateSphereMaterial = do
  chooseMat <- randomDouble
  case () of
    _
      | chooseMat < 0.8 -> do
          c1 <- randomColour
          MaterialWrapper . Lambertian . mulColours c1 <$> randomColour
      | chooseMat < 0.95 -> do
          albedo <- randomColourRange 0.5 1
          fuzz <- randomDoubleRange 0 0.5
          return $ MaterialWrapper $ Metal albedo fuzz
      | otherwise ->
          return $ MaterialWrapper $ Dielectric 1.5

generateSphere :: Double -> Double -> RayM (Maybe Sphere)
generateSphere a b = do
  rx <- randomDouble
  rz <- randomDouble
  let center' = Vec3 (a + 0.9 * rx) 0.2 (b + 0.9 * rz)
      comparePoint = Vec3 4 0.2 0

  if Vec3.length (Vec3.sub center' comparePoint) > 0.9
    then do
      Just . Sphere center' 0.2 <$> generateSphereMaterial
    else return Nothing

buildWorld :: RayM HittableList
buildWorld = do
  let groundSphere =
        Sphere
          { center = Vec3 0.0 (-1000) 0,
            radius = 1000.0,
            material = MaterialWrapper $ Lambertian $ Colour $ Vec3 0.5 0.5 0.5
          }

      sphere1 =
        Sphere
          { center = Vec3 0 1 0,
            radius = 1.0,
            material = MaterialWrapper $ Dielectric 1.5
          }

      sphere2 =
        Sphere
          { center = Vec3 (-4) 1 0,
            radius = 1.0,
            material = MaterialWrapper $ Lambertian $ Colour $ Vec3 0.4 0.2 0.1
          }

      sphere3 =
        Sphere
          { center = Vec3 4 1 0,
            radius = 1.0,
            material = MaterialWrapper $ Metal (Colour $ Vec3 0.7 0.6 0.5) 0.0
          }

  randomSpheres <-
    traverse (uncurry generateSphere) [(a, b) | a <- [-11 .. 11], b <- [-11 .. 11]]

  return $ fromList $ groundSphere : sphere1 : sphere2 : sphere3 : catMaybes randomSpheres
