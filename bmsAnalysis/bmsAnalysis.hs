import Control.Monad
import Text.Printf
import Control.Applicative

type KeysNum = (Int, Int, Int, Int, Int, Int, Int, Int)

main :: IO()
main = do
  contents <- getContents
  let (k1, k2, k3, k4, k5, k6, k7, scr) = bmsAnalysis $ lines contents
  printf "k1  : %d\n" k1
  printf "k2  : %d\n" k2
  printf "k3  : %d\n" k3
  printf "k4  : %d\n" k4
  printf "k5  : %d\n" k5
  printf "k6  : %d\n" k6
  printf "k7  : %d\n" k7
  printf "scr : %d\n" scr

bmsAnalysis :: [String] -> KeysNum
bmsAnalysis cs = bmsAnalysis' cs (0, 0, 0, 0, 0, 0, 0, 0) (0, 0, 0, 0, 0, 0, 0, 0) []
  where bmsAnalysis' [] ret lret _ = (0, 0, 0, 0, 0, 0, 0, 0)
        bmsAnalysis' (c:cs) ret lret lObj | take 6 c == "#LNOBJ" = objAnalysis (c:cs) ret lret ((drop 7 c):lObj)
                                          | elem ':' c = objAnalysis (c:cs) ret lret lObj
                                          | otherwise = bmsAnalysis' cs ret lret lObj

objAnalysis :: [String] -> KeysNum -> KeysNum -> [String] -> KeysNum
objAnalysis [] (k1, k2, k3, k4, k5, k6, k7, scr) (lk1, lk2, lk3, lk4, lk5, lk6, lk7, lscr) _ = (k1+(lk1 `div` 2), k2+(lk2 `div` 2), k3+(lk3 `div` 2), k4+(lk4 `div` 2), k5+(lk5 `div` 2), k6+(lk6 `div` 2), k7+(lk7 `div` 2), scr+(lscr `div` 2))
objAnalysis (c:cs) ret@(k1, k2, k3, k4, k5, k6, k7, scr) lret@(lk1, lk2, lk3, lk4, lk5, lk6, lk7, lscr) lObj | key == "11" = objAnalysis cs (k1+objCnt, k2, k3, k4, k5, k6, k7, scr) lret lObj
                                                                                                             | key == "12" = objAnalysis cs (k1, k2+objCnt, k3, k4, k5, k6, k7, scr) lret lObj
                                                                                                             | key == "13" = objAnalysis cs (k1, k2, k3+objCnt, k4, k5, k6, k7, scr) lret lObj
                                                                                                             | key == "14" = objAnalysis cs (k1, k2, k3, k4+objCnt, k5, k6, k7, scr) lret lObj
                                                                                                             | key == "15" = objAnalysis cs (k1, k2, k3, k4, k5+objCnt, k6, k7, scr) lret lObj
                                                                                                             | key == "18" = objAnalysis cs (k1, k2, k3, k4, k5, k6+objCnt, k7, scr) lret lObj
                                                                                                             | key == "19" = objAnalysis cs (k1, k2, k3, k4, k5, k6, k7+objCnt, scr) lret lObj
                                                                                                             | key == "16" = objAnalysis cs (k1, k2, k3, k4, k5, k6, k7, scr+objCnt) lret lObj
                                                                                                             | key == "51" = objAnalysis cs ret (lk1+lnObjCnt, lk2, lk3, lk4, lk5, lk6, lk7, lscr) lObj
                                                                                                             | key == "52" = objAnalysis cs ret (lk1, lk2+lnObjCnt, lk3, lk4, lk5, lk6, lk7, lscr) lObj
                                                                                                             | key == "53" = objAnalysis cs ret (lk1, lk2, lk3+lnObjCnt, lk4, lk5, lk6, lk7, lscr) lObj
                                                                                                             | key == "54" = objAnalysis cs ret (lk1, lk2, lk3, lk4+lnObjCnt, lk5, lk6, lk7, lscr) lObj
                                                                                                             | key == "55" = objAnalysis cs ret (lk1, lk2, lk3, lk4, lk5+lnObjCnt, lk6, lk7, lscr) lObj
                                                                                                             | key == "58" = objAnalysis cs ret (lk1, lk2, lk3, lk4, lk5, lk6+lnObjCnt, lk7, lscr) lObj
                                                                                                             | key == "59" = objAnalysis cs ret (lk1, lk2, lk3, lk4, lk5, lk6, lk7+lnObjCnt, lscr) lObj
                                                                                                             | key == "56" = objAnalysis cs ret (lk1, lk2, lk3, lk4, lk5, lk6, lk7, lscr+lnObjCnt) lObj
                                                                                                             | otherwise = objAnalysis cs ret lret lObj
  where (bar, key) =  splitAt 3 (tail barKey)
        objCnt = objNum objData lObj
        objData = tail objData'
        (barKey, objData') | elem ':' c = break (==':') c
                           | otherwise = ("#", ":")
        lnObjCnt = objNum objData lObj

objNum :: String -> [String] -> Int
objNum dt lObj = objNum' dt 0
  where  objNum' [] num = num
         objNum' (x:y:dt) num | (x == '0' && y == '0') || elem (x:y:[]) lObj = objNum' dt num
                              | otherwise = objNum' dt (num+1)  


