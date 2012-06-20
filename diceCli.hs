import System.SimpleArgs

import Dice

main :: IO ()
main = getArgs >>= evalDice . parseDice . (\s -> NumericS s) >>= print
