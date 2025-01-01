module Main where

data Scene = Scene
  { width :: Int,
    height :: Int
  }
  deriving (Show)

renderImage :: Scene -> String
renderImage s =
  let w = width s
      h = height s
   in unlines [renderPixel i j w h | j <- [0 .. h - 1], i <- [0 .. w - 1]]

renderPixel :: Int -> Int -> Int -> Int -> String
renderPixel i j w h =
  let r = fromIntegral i / fromIntegral (w - 1) :: Double
      g = fromIntegral j / fromIntegral (h - 1) :: Double
      b = 0.0 :: Double
      ir = floor (255.999 * r) :: Int
      ig = floor (255.999 * g) :: Int
      ib = floor (255.999 * b) :: Int
   in unwords [show ir, show ig, show ib]

main :: IO ()
main = do
  let scene = Scene {width = 256, height = 256}
      header = "P3\n" ++ show (width scene) ++ " " ++ show (height scene) ++ "\n255\n"
      body = renderImage scene
  writeFile "image.ppm" (header ++ body)
