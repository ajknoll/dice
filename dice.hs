import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Char
import Text.Parsec.Token
import Text.Parsec.Prim
import System.Environment
import System.Random (getStdGen, randomRs)

-- Parser
--
--expr :: Parser Integer
expr = buildExpressionParser table term
     <?> "dice set"

term = do {char '('; x <- expr; char ')'; return x}
   <|> number
   <?> "dice term"

count = 
  do
    c <- number <|> return 1
    char 'd' <|> char 'D'
    return c
  <|> return 1
  <?> "count"

size = number <?> "size"

table = [ [ binary "d" roll AssocLeft
          , binary "*" (*) AssocLeft
          , binary "/" div AssocLeft 
          , binary "+" (+) AssocLeft
          , binary "-" (-) AssocLeft
          ]
        ]
        where
          binary s f = Infix (do {string s; return f}) 

--number :: Parser Integer
number = do {ds <- many1 digit; return (read ds)} <?> "number"

-- Given a count and number of sides, produce count many random integers in the
-- range [1, sides]
-- 
roll :: Int -> Int -> IO Int
roll count sides = do
  g <- getStdGen
  let r = sum (take count (randomRs (1, sides) g))
  return r

main :: IO ()
main = getArgs >>= sequence . map (parseTest expr) >>= print
