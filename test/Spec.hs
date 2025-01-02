module Main (main) where

import Test.Hspec
import qualified Vec3Spec

main :: IO ()
main = hspec $ do
  describe "Vec3 Tests" Vec3Spec.spec
