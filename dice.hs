{-# LANGUAGE GADTs #-}

module Dice where

import System.SimpleArgs
import System.Random
import System.IO

type Sides = Int
type Count = Int

type Modifier t = (t -> t)

data Dice t where
  Numeric :: Count -> Sides -> [Modifier Int] -> Dice Int

instance Show t => Show (Dice t) where
  show (Numeric c s mods) = show c ++ "d" ++ show s -- ++ show ms -- TODO work out how to show mods

evalDice :: Dice t -> IO t
evalDice d@(Numeric c s mods) = do
  r <- roll d
  let m = applyMods (sum r) mods
  return m

roll :: Dice t -> IO [t]
roll (Numeric c s mods) = do
  g <- getStdGen
  let r = take c (randomRs (1, s) g)
  return r

applyMods :: t -> [Modifier t] -> t
applyMods = foldl (\acc mod -> mod acc)
