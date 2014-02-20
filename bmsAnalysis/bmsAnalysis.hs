-- OverloadedStrings 言語拡張を使うとダブルクオートで囲んだ文字列を、
-- Text、ByteString リテラルとして扱ってくれるようになります。 
{-# LANGUAGE OverloadedStrings #-} 

import Control.Monad
import System.Environment(getArgs)
import Text.Printf
import Bmsparser
import qualified Data.Text as T
import Data.List as L
import Data.Time
import System.Locale

main :: IO()
main = do
  args <- getArgs
  zonedTime <- getZonedTime
  let ymdString = formatTime defaultTimeLocale "%F" zonedTime
      hmsString = T.splitOn ":" $ T.pack $ formatTime defaultTimeLocale "%T" zonedTime
      timeString = concatMap T.unpack $ intersperse "-" hmsString
      outFilename = "bmsData_" ++ ymdString ++ "_" ++timeString ++ ".tsv"
  appendFile outFilename "%s\t%d\t%d\t%.2f\t%d\t%.2f\n"
  forM_ args $ \readFilePath -> do
    contents <- readFile readFilePath
    let title = getMusicTitle contents
        totalValue = getTotalValue contents
        (k1, k2, k3, k4, k5, k6, k7, scr) = getCntObjKeys contents
        total = k1 + k2 + k3 + k4 + k5 + k6 + k7 + scr
        scrPerTotal = (read (show scr) :: Float) / (read (show total) :: Float)
        totalValPerTotal = (read (show totalValue) :: Float) / (read (show total) :: Float)
        str = printf "%s\t%d\t%d\t%.2f\t%d\t%.2f\n" title scr total scrPerTotal totalValue totalValPerTotal
    appendFile outFilename str
--    printf "%s\t%d\t%d\t%d\t%d\n" title scr total scrPerTotal totalValPerTotal
--    print $ show title ++ "\t" ++ show scr ++ "\t" ++ show scrPerTotal ++ "\t" ++ show totalValue
