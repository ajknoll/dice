import Dice
import System.SimpleArgs

main :: IO ()
main = getArgs >>= evalDice . parseDice . (\s -> NumericS s) >>= print
