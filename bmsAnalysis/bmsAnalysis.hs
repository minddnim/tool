import Control.Monad()
import Text.Printf
import Control.Applicative()
import Data.Map as M
import Data.List as L
import System.Environment(getArgs)

type Field = Map String Int

(key1, key2, key3, key4, key5, key6, key7, scr) = ("11","12","13","14","15","18","19","16")
(lkey1, lkey2, lkey3, lkey4, lkey5, lkey6, lkey7, lscr) = ("51","52","53","54","55","58","59","56")

initObj = M.fromList [(key1, 0),  (key2, 0),  (key3, 0),  (key4, 0),  (key5, 0),  (key6, 0),  (key7, 0),  (scr, 0),
                      (lkey1, 0), (lkey2, 0), (lkey3, 0), (lkey4, 0), (lkey5, 0), (lkey6, 0), (lkey7, 0), (lscr, 0)]

main :: IO()
main = do
  args <- getArgs
  contents <- (readFile (head args))
  let objData = bmsAnalysis (lines contents) []
  printf "k1  : %d\n" $ (objData ! key1) + (objData ! lkey1 `div` 2)
  printf "k2  : %d\n" $ (objData ! key2) + (objData ! lkey2 `div` 2)
  printf "k3  : %d\n" $ (objData ! key3) + (objData ! lkey3 `div` 2)
  printf "k4  : %d\n" $ (objData ! key4) + (objData ! lkey4 `div` 2)
  printf "k5  : %d\n" $ (objData ! key5) + (objData ! lkey5 `div` 2)
  printf "k6  : %d\n" $ (objData ! key6) + (objData ! lkey6 `div` 2)
  printf "k7  : %d\n" $ (objData ! key7) + (objData ! lkey7 `div` 2)
  printf "scr : %d\n" $ (objData ! scr)  + (objData ! lscr  `div` 2)
  a <- getLine
  print a

-- bmsファイルからbmsデータ解析
bmsAnalysis :: [String] -> [String] -> Field
bmsAnalysis [] _ = initObj
bmsAnalysis (c:cs) lnIdList | isLnDef = bmsAnalysis cs (lnId:lnIdList)
                            | isEmptyList = bmsAnalysis cs lnIdList
                            | isFieldDef = laneAnalysis (c:cs) lnIdList initObj
                            | otherwise = bmsAnalysis cs lnIdList
  where isLnDef = take 6 c == "#LNOBJ"
        isEmptyList = L.null c
        isFieldDef = (head c == '#') && (last (take 7 c)) == ':'
        lnId = drop 7 c

-- #xxxyy:zzzzzzzzzzzzzzzzという文字列を解析 xは小節数, yはkeyId, zはオブジェ情報
laneAnalysis :: [String] -> [String] -> Field -> Field
laneAnalysis [] _ objMap = objMap
laneAnalysis (c:cs) lnIdList objMap = laneAnalysis cs lnIdList updateObj
  where keyId = take 2 $ drop 4 c
        obInfo = createObjInfo $ drop 7 c
        objNum = cntObj obInfo lnIdList
        updateObj = insertWith (+) keyId objNum objMap

-- 1小節に定義されているオブジェ情報の作成
createObjInfo :: String -> [String]
createObjInfo dt = createObjInfo' dt []
  where createObjInfo' [] obInfo = obInfo
        createObjInfo' dat@(_:_:xs) obInfo = createObjInfo' xs ((take 2 dat) : obInfo)
        createObjInfo' _ _ = []

-- オブジェ情報から、配置されているオブジェ数をカウント(拡張定義されたLNオブジェを除く)
cntObj :: [String] -> [String] -> Int
cntObj obInfo lObj = length $ L.foldr (delLnObj) [] obData
  where obData = L.filter (/="00") obInfo
        delLnObj x y | elem x lObj = y
                     | otherwise = (x:y)
 
