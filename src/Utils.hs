{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Utils where

import Control.Monad.State
import System.Random

newtype RayM a = RayM (State StdGen a)
  deriving (Functor, Applicative, Monad, MonadState StdGen)

runRayM :: RayM a -> StdGen -> (a, StdGen)
runRayM (RayM s) = runState s

randomDouble :: RayM Double
randomDouble = do
  gen <- get
  let (value, newGen) = randomR (0.0, 1.0) gen
  put newGen
  return value

randomDoubleRange :: Double -> Double -> RayM Double
randomDoubleRange min' max' = do
  r <- randomDouble
  return $ min' + (max' - min') * r

degreesToRadians :: Double -> Double
degreesToRadians degrees = degrees * pi / 180
