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

rollP :: Parser (Integer, Integer)
rollP = 
  do
    count <- intP <|> return 1
    oneOf "dD"
    size  <- intP
    return (count, size) 
  <|> return 1
  <?> "dice"

operationP :: Parser (Integer -> Integer)
operationP =
  do
    op <- oneOf "+-*/"
    n  <- intP
    return $ case op of
                  '+' -> (+)
                  '-' -> (-)
                  '*' -> (-)
                  '/' -> (-)
                  _   -> id
             $ n
  <?> "operation"

intP :: Parser Integer
intP = do {ds <- many1 digit; return (read ds)} <?> "integer"

-- Given a count and number of sides, produce count many random integers in the
-- range [1, sides]
-- 
roll :: Integer -> Integer -> IO Integer
roll count sides = do
  g <- getStdGen
  let r = sum (take count (randomRs (1, sides) g))
  return r

main :: IO ()
main = getArgs >>= sequence . map (parseTest expr) >>= print
