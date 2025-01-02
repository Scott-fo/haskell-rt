module Main where

import Colour (writeColour)
import Vec3 (Vec3 (Vec3))

data Scene = Scene
  { width :: Int,
    height :: Int
  }
  deriving (Show)

renderImage :: Scene -> String
renderImage s =
  let w = width s
      h = height s
   in unlines
        [ writeColour (Vec3 r g 0)
          | j <- [0 .. h - 1],
            i <- [0 .. w - 1],
            let r = fromIntegral i / fromIntegral (w - 1)
                g = fromIntegral j / fromIntegral (h - 1)
        ]

main :: IO ()
main = do
  let scene = Scene {width = 256, height = 256}
      header = "P3\n" ++ show (width scene) ++ " " ++ show (height scene) ++ "\n255\n"
      body = renderImage scene
  writeFile "image.ppm" (header ++ body)
