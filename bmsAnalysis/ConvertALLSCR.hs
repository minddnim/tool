import Control.Monad
import Control.Applicative()
import System.Environment(getArgs)
import System.FilePath as F
import System.FilePath.Posix as P
import Text.Printf
import Bmsparser

main :: IO()
main = do
  args <- getArgs
  forM_ args $ \readFilePath -> do
    contents <- readFile readFilePath
    let filePath = getDirPath readFilePath
        writeFileName = "sara_" ++ F.takeFileName readFilePath
        writeFileNameBme = filePath ++ P.replaceExtension writeFileName "bme"
        convBmsData = convAllScr contents
    writeFile writeFileNameBme convBmsData
--    printf "%s" filePath

getDirPath :: String -> String
getDirPath str = reverse $ dropWhile ('\\' /=) $ reverse str