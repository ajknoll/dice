{-# LANGUAGE GADTs #-}

module Dice where

import qualified Data.Text as T
import Data.List.Split
import System.Random (getStdGen, randomRs)
import System.SimpleArgs
import System.IO

-- Constants
-- 
countSeparators = "dD"
modOperators = "+-*/"

-- Types
--
type Sides = Int
type Count = Int

data Modifier t where
  MapInt :: (Int -> Int) -> Modifier Int

data Dice t where
  NumericD :: Count -> Sides -> [Modifier Int] -> Dice Int

data DiceString t where
  NumericS :: String -> DiceString Int

-- Does not show modifiers.
instance Show t => Show (Dice t) where
  show (NumericD c s mods) = show c ++ "d" ++ show s

-- Given a Dice instance, produce the result of evaluation, including random
-- rolls.
-- 
evalDice :: Dice t -> IO t
evalDice d@(NumericD c s mods) = do
  r <- roll d
  let m = applyMods (sum r) mods
  return m

-- Given a Dice instance, produce the relevant list of random objects.
-- 
roll :: Dice t -> IO [t]
roll (NumericD c s mods) = do
  g <- getStdGen
  let r = take c (randomRs (1, s) g)
  return r

-- Applies a list of Modifiers sequentially to its input.
--
applyMods :: t -> [Modifier t] -> t
applyMods x mods = foldl (\acc mod -> mod acc) x (map extract mods)
  where extract (MapInt m) = m

parseDice :: DiceString t -> Dice t
parseDice (NumericS s) =
  NumericD count sides mods
  where 
    splitOnD = splitOneOf countSeparators s
    hasCount = length splitOnD > 1 && (not . null . head) splitOnD
    count = if hasCount then (read . head) splitOnD else 1
    splitOnOps = (split . oneOf) modOperators (last splitOnD) -- preserve the delimiters this time
    sides = (read . head) splitOnOps
    mods = parseMods [MapInt (\x -> x)] (tail splitOnOps) -- initial mod is a hack to get correct type recognition from parseMods

parseMods :: [Modifier t] -> [String] -> [Modifier t]
parseMods mods [] = mods
parseMods mods@(MapInt _:_) (op:x:rest) = parseMods (mods ++ [newMod]) rest
  where 
    xVal = read x :: Int
    newMod = case (op) of
                  "+" -> MapInt (\y -> y + xVal)
                  "-" -> MapInt (\y -> y - xVal)
                  "*" -> MapInt (\y -> y * xVal)
                  "/" -> MapInt (\y -> y `div` xVal)
                  otherwise -> MapInt (\x -> x)
parseMods mods _ = mods


--main = getArgs >>= parseDice >>= map evalDice >>= print
