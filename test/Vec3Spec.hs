module Vec3Spec (spec) where

import Test.Hspec
import Vec3

spec :: Spec
spec = do
  describe "Vec3 operations" $ do
    it "adds two vectors" $ do
      add (Vec3 1 2 3) (Vec3 4 5 6) `shouldBe` Vec3 5 7 9

    it "subtracts two vectors" $ do
      sub (Vec3 1 2 3) (Vec3 4 5 6) `shouldBe` Vec3 (-3) (-3) (-3)

    it "scales a vector by a scalar" $ do
      mul (Vec3 1 2 3) 2 `shouldBe` Vec3 2 4 6

    it "divides a vector by a scalar" $ do
      Vec3.div (Vec3 2 4 6) 2 `shouldBe` Just (Vec3 1 2 3)

    it "calculates the length of a vector" $ do
      Vec3.length (Vec3 3 4 0) `shouldBe` 5

    it "calculates the dot product" $ do
      dot (Vec3 1 2 3) (Vec3 4 5 6) `shouldBe` 32

    it "calculates the cross product" $ do
      cross (Vec3 1 0 0) (Vec3 0 1 0) `shouldBe` Vec3 0 0 1

    it "normalizes a vector" $ do
      unitVector (Vec3 3 0 0) `shouldBe` Just (Vec3 1 0 0)
