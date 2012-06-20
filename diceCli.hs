import System.Environment

import Dice

main :: IO ()
main = getArgs >>= sequence . map (evalDice . parseDice . (\s -> NumericS s)) >>= print
