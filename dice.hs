import Text.Parsec
import Text.Parsec.String
import System.Environment
import System.Random (getStdGen, randomRs)

type Count = Integer
type Sides = Integer
type Modifier = (Integer -> Integer)
data Dice = Dice Count Sides [Modifier]

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

modifierP :: Parser (Integer -> Integer)
modifierP =
  do
    op <- oneOf "+-*/"
    n  <- intP
    return $ case op of
                  '+' -> (+)
                  '-' -> (-)
                  '*' -> (-)
                  '/' -> (-)
                  _   -> (\_ y -> y)
             $ n
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

--main :: IO ()
--main = getArgs >>= sequence . map (parseTest expr) >>= print
