module Main where

import Camera (render)
import Control.Exception (IOException, try)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import HittableList (fromList)
import Sphere (Sphere (Sphere))
import System.IO (hPutStrLn, stderr)
import Vec3 (Vec3 (Vec3))

main :: IO ()
main = do
  let sphere1 = Sphere (Vec3 0 0 (-1)) 0.5
      sphere2 = Sphere (Vec3 0 (-100.5) (-1)) 100

      world = fromList [sphere1, sphere2]

  image <- render world

  result <- try $ TIO.writeFile "image.ppm" (T.pack image)
  case result of
    Left e -> hPutStrLn stderr $ "Error writing to file: " ++ show (e :: IOException)
    Right _ -> putStrLn "Image successfully written to image.ppm"
