{-# LANGUAGE RecordWildCards #-}

module Camera
  ( Camera (Camera),
    aspectRatio,
    imageWidth,
    vfov,
    initialize,
    render,
  )
where

import Colour (Colour, addColours, black, blue, mulColour, mulColours, sumColours, white, writeColour)
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (replicateM)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Hittable (HitRecord (mat), Hittable (hit), Material (scatter), MaterialWrapper (MaterialWrapper), ScatterResult (ScatterResult, attenuation, scattered))
import HittableList (HittableList)
import Interval (unsafeMkInterval)
import Ray (Ray (Ray, direction))
import System.Random.Stateful (mkStdGen)
import Utils (RayM, degreesToRadians, randomDoubleRange, runRayM)
import Vec3 (Point3, Vec3 (Vec3), add, cross, mul, negative, randomInUnitDisk, sub, unitVector)

data Camera = Camera
  { aspectRatio :: Double,
    imageWidth :: Int,
    samplesPerPixel :: Int,
    maxDepth :: Int,
    vfov :: Double,
    defocusAngle :: Double,
    focusDist :: Double,
    lookFrom :: Point3,
    lookAt :: Point3,
    vup :: Vec3,
    imageHeight :: Int,
    center :: Point3,
    pixel00Loc :: Point3,
    pixelDeltaU :: Vec3,
    pixelDeltaV :: Vec3,
    defocusDiskU :: Vec3,
    defocusDiskV :: Vec3
  }
  deriving (Show)

initialize :: Camera
initialize = do
  let aspectRatio = 16.0 / 9.0 :: Double
      imageWidth = 1200 :: Int
      imageHeight = floor (fromIntegral imageWidth / aspectRatio)

      samplesPerPixel = 10
      maxDepth = 50

      vfov = 20
      lookFrom = Vec3 13 2 3 :: Point3
      lookAt = Vec3 0 0 0 :: Point3
      vup = Vec3 0 1 0

      defocusAngle = 0.6
      focusDist = 10 :: Double

      center = lookFrom

      theta = degreesToRadians vfov
      h = tan (theta / 2)

      viewportHeight = 2.0 * h * focusDist
      viewportWidth = viewportHeight * (fromIntegral imageWidth / fromIntegral imageHeight)

      w = fromMaybe (error "Failed to initialise camera") (unitVector $ Vec3.sub lookFrom lookAt)
      u = fromMaybe (error "Failed to initialise camera") (unitVector $ cross vup w)
      v = cross w u

      viewportU = mul u viewportWidth
      viewportV = mul (negative v) viewportHeight

      pixelDeltaU = mul viewportU (1 / fromIntegral imageWidth)
      pixelDeltaV = mul viewportV (1 / fromIntegral imageHeight)

      viewportUpperLeft =
        foldl'
          sub
          center
          [ mul w focusDist,
            mul viewportU 0.5,
            mul viewportV 0.5
          ]

      pixel00Loc = viewportUpperLeft `Vec3.add` ((pixelDeltaU `Vec3.add` pixelDeltaV) `mul` 0.5)

      defocusRadius = focusDist * tan (degreesToRadians (defocusAngle / 2))
      defocusDiskU = mul u defocusRadius
      defocusDiskV = mul v defocusRadius
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
  return $ mulColour (sumColours samples) pixelScale

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
getRay camera@Camera {..} i j = do
  offsetX <- randomDoubleRange (-0.5) 0.5
  offsetY <- randomDoubleRange (-0.5) 0.5
  sample <- defocusDiskSample camera

  let pixelSample =
        foldl'
          Vec3.add
          pixel00Loc
          [ mul pixelDeltaU (fromIntegral i + offsetX),
            mul pixelDeltaV (fromIntegral j + offsetY)
          ]

  let rayOrigin = if defocusAngle <= 0 then center else sample
      rayDirection = pixelSample `sub` rayOrigin
  return $ Ray rayOrigin rayDirection

defocusDiskSample :: Camera -> RayM Point3
defocusDiskSample Camera {..} = do
  (Vec3 x y _) <- randomInUnitDisk
  return $ foldl' Vec3.add center [mul defocusDiskU x, mul defocusDiskV y]

rayColour :: Ray -> Int -> HittableList -> RayM Colour
rayColour ray depth world
  | depth <= 0 = return black
  | otherwise = do
      maybe
        (return $ backgroundColour ray)
        (handleHit ray depth world)
        (hit world ray (unsafeMkInterval 0.0001 infinity))

handleHit :: Ray -> Int -> HittableList -> HitRecord -> RayM Colour
handleHit ray depth world rec = do
  scatterResult <- scatterFromMaterial (mat rec) ray rec
  maybe
    (return black)
    (handleScatter depth world)
    scatterResult

handleScatter :: Int -> HittableList -> ScatterResult -> RayM Colour
handleScatter depth world ScatterResult {..} = do
  bounceColour <- rayColour scattered (depth - 1) world
  return $ mulColours attenuation bounceColour

scatterFromMaterial :: MaterialWrapper -> Ray -> HitRecord -> RayM (Maybe ScatterResult)
scatterFromMaterial (MaterialWrapper material) = scatter material

backgroundColour :: Ray -> Colour
backgroundColour Ray {..} = fromMaybe black $ do
  Vec3 _ y _ <- unitVector direction
  let t = 0.5 * (y + 1.0)
  return $ addColours (mulColour white (1.0 - t)) (mulColour blue t)

ppmHeader :: Int -> Int -> String
ppmHeader w h =
  "P3\n" ++ show w ++ " " ++ show h ++ "\n255\n"

infinity :: Double
infinity = 1 / 0
