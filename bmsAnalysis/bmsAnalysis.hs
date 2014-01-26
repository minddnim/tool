import Control.Monad()
import Control.Applicative()
import System.Environment(getArgs)
import Text.Printf
import Bmsparser

main :: IO()
main = do
  args <- getArgs
  contents <- readFile $ head args
--  printf "%s" $ show $ convAllScr' $ getMainDataField contents
  printf "%s" $ convAllScr $ getMainDataField contents
