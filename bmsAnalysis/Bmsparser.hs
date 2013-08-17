{-# LANGUAGE OverloadedStrings #-}

module Bmsparser
( CntObjKey(..)
, getCntObjKey
) where

import Data.ByteString.Char8 as B (ByteString, unpack, pack)
import Data.Attoparsec.Char8
import Data.Word
import System.IO
import Control.Applicative
import Data.Map as M
import Data.List as L

data FieldData = FieldData { channel :: String
                           , keyID :: String
                           , objs :: [String]
                           } deriving (Show)

data CntObjKey = CntObjKey { nKey1 :: Int
                           , nKey2 :: Int
                           , nKey3 :: Int
                           , nKey4 :: Int
                           , nKey5 :: Int
                           , nKey6 :: Int
                           , nKey7 :: Int
                           , nScr  :: Int
                           } deriving (Show)

type Field = Map String Int

-- 各キーのkeyIDの割り当て
(k1, k2, k3, k4, k5, k6, k7, scr) = ("11","12","13","14","15","18","19","16")
(lk1, lk2, lk3, lk4, lk5, lk6, lk7, lscr) = ("51","52","53","54","55","58","59","56")

-- 各キーのオブジェの個数をカウント
initObj = M.fromList [(k1, 0),  (k2, 0),  (k3, 0),  (k4, 0),  (k5, 0),  (k6, 0),  (k7, 0),  (scr, 0),
                      (lk1, 0), (lk2, 0), (lk3, 0), (lk4, 0), (lk5, 0), (lk6, 0), (lk7, 0), (lscr, 0)]

getCntObjKey :: [String] -> CntObjKey
getCntObjKey cs = CntObjKey oKey1 oKey2 oKey3 oKey4 oKey5 oKey6 oKey7 oScr
  where fld = bmsAnalysis cs []
        oKey1 = (fld ! k1)  + (fld ! lk1 `div` 2)
        oKey2 = (fld ! k2)  + (fld ! lk2 `div` 2)
        oKey3 = (fld ! k3)  + (fld ! lk3 `div` 2)
        oKey4 = (fld ! k4)  + (fld ! lk4 `div` 2)
        oKey5 = (fld ! k5)  + (fld ! lk5 `div` 2)
        oKey6 = (fld ! k6)  + (fld ! lk6 `div` 2)
        oKey7 = (fld ! k7)  + (fld ! lk7 `div` 2)
        oScr  = (fld ! scr) + (fld ! lscr `div` 2)

-- bmsファイルからbmsデータ解析
bmsAnalysis :: [String] -> [String] -> Field
bmsAnalysis [] _ = initObj
bmsAnalysis (c:cs) lnIdList | isLnDef = bmsAnalysis cs (lnId:lnIdList)
                            | isEmptyList = bmsAnalysis cs lnIdList
                            | isFieldDef = laneAnalysis (c:cs) lnIdList initObj
                            | otherwise = bmsAnalysis cs lnIdList
  where isLnDef = Prelude.take 6 c == "#LNOBJ"
        isEmptyList = L.null c
        isFieldDef = (head c == '#') && (last (Prelude.take 7 c)) == ':'
        lnId = drop 7 c

-- #xxxyy:zzzzzzzzzzzzzzzzという文字列を解析 xは小節数, yはkeyId, zはオブジェ情報
laneAnalysis :: [String] -> [String] -> Field -> Field
laneAnalysis [] _ objMap = objMap
laneAnalysis (c:cs) lnIdList objMap = laneAnalysis cs lnIdList updateObj
  where fData = getFieldData c
        keyId = keyID fData
        obInfo = objs fData
        objNum = cntObj obInfo lnIdList
        updateObj = insertWith (+) keyId objNum objMap

-- オブジェ情報から、配置されているオブジェ数をカウント(拡張定義されたLNオブジェを除く)
cntObj :: [String] -> [String] -> Int
cntObj obInfo lObj = length $ L.foldr (delLnObj) [] obData
  where obData = L.filter (/="00") obInfo
        delLnObj x y | elem x lObj = y
                     | otherwise = (x:y)

-- #xxxyy:zzzzzzzzという文字列から"xxx" "yy" ["zz", "zz", "zz", "zz"] というデータを得る
getFieldData :: String -> FieldData
getFieldData str =
  case result of
    Left _ -> error "bmsParser error."
    Right a -> a
  where result = parseOnly bmsParser (B.pack str)

-- #xxxyy:zzzzzzzzという文字列を解析 xは小節数, yはkeyId, zはオブジェ情報
bmsParser :: Parser FieldData
bmsParser = do
  char '#'
  channel <- count 3 anyChar  
  keyId <- count 2 anyChar
  char ':'
  objs <- many $ count 2 anyChar
  return $ FieldData channel keyId objs

