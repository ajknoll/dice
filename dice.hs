{-# LANGUAGE GADTs #-}

import Text.Parsec
import Text.Parsec.String
import System.Environment
import System.Random (getStdGen, randomRs)

type Count = Integer
type Sides = Integer

data Modifier where
  Add :: Integer -> Modifier
  Sub :: Integer -> Modifier
  Mul :: Integer -> Modifier
  Div :: Integer -> Modifier
  Custom :: (Integer -> Integer) -> Modifier

instance Show Modifier where
  show (Add x) = "+" ++ show x
  show (Sub x) = "-" ++ show x
  show (Mul x) = "*" ++ show x
  show (Div x) = "/" ++ show x
  show (Custom _) = "?"

data Dice = Dice Count Sides [Modifier]

instance Show Dice where
  show (Dice c s mods) = show c ++ "d" ++ show s ++ show mods


diceP :: Parser Dice
diceP =
  do
    (c, s) <- rollP
    mods <- many modifierP
    return $ Dice c s mods
  <?> "dice"

rollP :: Parser (Integer, Integer)
rollP = 
  do
    c <- intP <|> return 1
    oneOf "dD"
    s <- intP
    return (c, s) 
  <?> "roll"

modifierP :: Parser Modifier
modifierP =
  do
    op <- oneOf "+-*/"
    n  <- intP
    return $ case op of
                  '+' -> Add n
                  '-' -> Sub n
                  '*' -> Mul n
                  '/' -> Div n
                  _ -> Custom id
  <?> "modifier"

intP :: Parser Integer
intP = do {ds <- many1 digit; return (read ds)} <?> "integer"

-- Given a count and number of sides, produce count many random integers in the
-- range [1, sides]
-- 
roll :: Integer -> Integer -> IO Integer
roll c s = do
  g <- getStdGen
  let r = sum (take (fromIntegral c) (randomRs (1, s) g))
  return r

applyMod :: Modifier -> Integer -> Integer
applyMod (Add x) i = x + i
applyMod (Sub x) i = x - i
applyMod (Mul x) i = x * i
applyMod (Div x) i = x `div` i
applyMod (Custom f) i = f i

evalDice :: Dice -> IO Integer
evalDice (Dice c s mods) = do
  r <- roll c s
  return $ foldr applyMod r mods

main :: IO ()
main = getArgs >>= mapM (parseTest diceP) >>= print
