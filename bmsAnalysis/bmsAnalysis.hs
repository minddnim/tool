import Control.Monad()
import Text.Printf
import Control.Applicative()
import System.Environment(getArgs)
import Bmsparser

main :: IO()
main = do
  args <- getArgs
  contents <- (readFile (head args))
  let objData = getCntObjKey (lines contents)
  printf "k1  : %d\n" $ nKey1 objData
  printf "k2  : %d\n" $ nKey2 objData
  printf "k3  : %d\n" $ nKey3 objData
  printf "k4  : %d\n" $ nKey4 objData
  printf "k5  : %d\n" $ nKey5 objData
  printf "k6  : %d\n" $ nKey6 objData
  printf "k7  : %d\n" $ nKey7 objData
  printf "scr : %d\n" $ nScr objData
  a <- getLine
  print a


