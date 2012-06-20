import Dice
import System.SimpleArgs

main :: IO ()
main = getArgs >>= mapM_ (print . evalDice . parseDice . (\s -> NumericS s))
