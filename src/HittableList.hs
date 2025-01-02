{-# LANGUAGE ExistentialQuantification #-}

module HittableList where

import Data.List (foldl')
import Hittable (Hittable (hit), t)

data SomeHittable = forall a. (Hittable a) => SomeHittable a

newtype HittableList = HittableList [SomeHittable]

emptyHittableList :: HittableList
emptyHittableList = HittableList []

addHittable :: (Hittable a) => HittableList -> a -> HittableList
addHittable (HittableList objects) obj = HittableList (SomeHittable obj : objects)

instance Hittable HittableList where
  hit (HittableList objects) ray tmin tmax = foldl' closestHit Nothing objects
    where
      closestHit acc (SomeHittable obj) =
        case hit obj ray tmin tmax of
          Nothing -> acc
          Just tempRec ->
            case acc of
              Nothing -> Just tempRec
              Just closestRec ->
                if t tempRec < t closestRec
                  then Just tempRec
                  else Just closestRec
