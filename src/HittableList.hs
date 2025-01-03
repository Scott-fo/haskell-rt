{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HittableList
  ( HittableList,
    SomeHittable (..),
    empty,
    singleton,
    fromList,
    add,
  )
where

import Data.List (foldl')
import Hittable (HitRecord (hitT), Hittable (hit))
import Interval (Interval)
import Ray (Ray)

data SomeHittable = forall a. (Hittable a) => SomeHittable a

newtype HittableList = HittableList [SomeHittable]
  deriving (Semigroup, Monoid)

empty :: HittableList
empty = HittableList []

singleton :: (Hittable a) => a -> HittableList
singleton = HittableList . pure . SomeHittable

fromList :: (Hittable a) => [a] -> HittableList
fromList = HittableList . map SomeHittable

add :: (Hittable a) => a -> HittableList -> HittableList
add obj (HittableList objects) = HittableList (SomeHittable obj : objects)

instance Hittable HittableList where
  hit (HittableList objects) ray interval =
    foldl' (closestHit ray interval) Nothing objects

closestHit ::
  Ray ->
  Interval ->
  Maybe HitRecord ->
  SomeHittable ->
  Maybe HitRecord
closestHit ray interval acc (SomeHittable obj) =
  case (acc, hit obj ray interval) of
    (Nothing, hit') -> hit'
    (Just closest, Just candidate)
      | hitT candidate < hitT closest -> Just candidate
      | otherwise -> Just closest
    (Just closest, Nothing) -> Just closest
