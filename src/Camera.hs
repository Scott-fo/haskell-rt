{-# LANGUAGE RecordWildCards #-}

module Camera
  ( Camera (Camera),
    aspectRatio,
    imageWidth,
    initialize,
    render,
  )
where

import Colour (Colour (Colour), mkColour, mulColour, sumColours, writeColour)
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (replicateM)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Hittable (HitRecord (HitRecord, hitPoint, normal), Hittable (hit))
import HittableList (HittableList)
import Interval (unsafeMkInterval)
import Ray (Ray (Ray, direction))
import System.Random.Stateful (mkStdGen)
import Utils (RayM, randomDoubleRange, runRayM)
import Vec3 (Point3, Vec3 (Vec3), add, mul, randomUnitVec3, sub, unitVector)

data Camera = Camera
  { aspectRatio :: Double,
    imageWidth :: Int,
    samplesPerPixel :: Int,
    maxDepth :: Int,
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

      samplesPerPixel = 100
      maxDepth = 50

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

splitIntoChunks :: Int -> Int -> [(Int, Int)]
splitIntoChunks numChunks total =
  let chunkSize = total `div` numChunks
      remainder = total `mod` numChunks
      sizes = replicate remainder (chunkSize + 1) ++ replicate (numChunks - remainder) chunkSize
      starts = scanl (+) 0 sizes
   in zip starts (tail starts)

generatePixelCoords :: Int -> Int -> Int -> [(Int, Int)]
generatePixelCoords imageWidth start end =
  [(i, j) | j <- [start .. end - 1], i <- [0 .. imageWidth - 1]]

samplePixel :: Camera -> HittableList -> (Int, Int) -> RayM Colour
samplePixel camera@Camera {..} world (i, j) = do
  samples <- replicateM samplesPerPixel $ do
    ray <- getRay camera i j
    rayColour ray maxDepth world

  let pixelScale = 1.0 / fromIntegral samplesPerPixel
  return $ mulColour pixelScale (sumColours samples)

renderChunk :: Camera -> HittableList -> (Int, Int) -> IO String
renderChunk camera world (start, end) = do
  let initialGen = mkStdGen (42 + start)
      pixels = generatePixelCoords (imageWidth camera) start end
      (colours, _) = runRayM (mapM (samplePixel camera world) pixels) initialGen
  return $ unlines (map writeColour colours)

render :: HittableList -> IO String
render world = do
  numCores <- getNumCapabilities
  let camera = initialize
      chunks = splitIntoChunks numCores (imageHeight camera)
      header = ppmHeader (imageWidth camera) (imageHeight camera)

  chunkResults <- mapConcurrently (renderChunk camera world) chunks
  return $ header ++ concat chunkResults

getRay :: Camera -> Int -> Int -> RayM Ray
getRay Camera {..} i j = do
  offsetX <- randomDoubleRange (-0.5) 0.5
  offsetY <- randomDoubleRange (-0.5) 0.5

  let pixelSample =
        foldl'
          Vec3.add
          pixel00Loc
          [ mul pixelDeltaU (fromIntegral i + offsetX),
            mul pixelDeltaV (fromIntegral j + offsetY)
          ]

  return $ Ray center (pixelSample `sub` center)

rayColour :: Ray -> Int -> HittableList -> RayM Colour
rayColour ray depth world
  | depth <= 0 = return $ Colour $ Vec3 0 0 0
  | otherwise =
      maybe
        (return $ backgroundColour ray)
        (\rec -> sphereColour rec depth world)
        (hit world ray (unsafeMkInterval 0.001 infinity))

sphereColour :: HitRecord -> Int -> HittableList -> RayM Colour
sphereColour HitRecord {..} depth world = do
  direction <- randomUnitVec3
  colour <- rayColour (Ray hitPoint (direction `Vec3.add` normal)) (depth - 1) world
  return $ mulColour 0.5 colour

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
