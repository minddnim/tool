import Control.Monad()
import Control.Applicative()
import System.Environment(getArgs)
import Bmsparser

main :: IO()
main = do
  args <- getArgs
  contents <- readFile $ head args
  print $ getMainDataField contents
  a <- getLine
  print a
