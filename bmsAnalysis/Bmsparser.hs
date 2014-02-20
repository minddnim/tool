{-# LANGUAGE OverloadedStrings #-}

{-
bmsファイルの構造について・・・
bmsファイルは大きく分けて
ヘッダーフィールドとメインデータフィールドがある
ヘッダーフィールドは曲名やキー音の定義、BGMの定義等が記述されている
メインデータフィールドは、どの小節のどの位置に何のキー音を再生するかという情報が書かれている
(簡単に言うとオブジェの位置定義)

メインデータフィールドの構造について書く
各キーのIDは以下の通り
(k1, k2, k3, k4, k5, k6, k7, scr) = ("11","12","13","14","15","18","19","16")

ロングノート時の各キーのIDは以下の通り
(lk1, lk2, lk3, lk4, lk5, lk6, lk7, lscr) = ("51","52","53","54","55","58","59","56")

オブジェ定義のフォーマットとしては以下のような仕様である。
#00111:07080708
これは001小節目のキーID11番(つまり1番ボタン)に4分のリズムでオブジェが4個降ってくる
#の次の3つ分の数字は小節数、次の2文字はキーID、
「:」以降の数字は2個で1セットとなっていて、
その1セットが01からZZまでの文字列のときにオブジェと認識。

-}

module Bmsparser
(
  convAllScr,
  getMainDataField,
  getMusicTitle,
  getCntObjKeys,
  getTotalValue
) where

import qualified Data.List as L (zip, partition, length, filter, isPrefixOf)
import qualified Data.Map.Lazy as M
import Data.Text as T (Text, length, chunksOf, pack, unpack, append, concat)
import Data.Attoparsec.Text as T
import Control.Applicative

data MainDataField = MainDataField {
  getBar :: Text,
  getKId :: Text,
  getRhythm :: Int,
  getObjs :: [ObjInfo]
} deriving (Show)

type ObjInfo = (Pos, WavId)
type Pos = Int
type WavId = Text

-- HeaderField Process
data WavDef = WavDef {
  getWavId :: Text,
  getWavFileName :: Text
}

data HeaderField = HeaderField {
  getGenre :: Text,
  getTitle :: Text,
  getArtist :: Text,
  getBpm :: Text,
  getPlayLevel :: Text,
  getRank :: Text,
  getTotal :: Text,
  getWavDefSet :: [WavDef]
}

(k1, k2, k3, k4, k5, k6, k7, scr) = ("11","12","13","14","15","18","19","16")
(lk1, lk2, lk3, lk4, lk5, lk6, lk7, lscr) = ("51","52","53","54","55","58","59","56")

-- 鍵盤と皿のID
ksId :: [Text]
ksId = [k1,k2,k3,k4,k5,k6,k7,scr]

-- 鍵盤と皿のID(ロングノート)
lksId :: [Text]
lksId = [lk1,lk2,lk3,lk4,lk5,lk6,lk7,lscr]


title :: Parser Text
title = string "#TITLE"

artist :: Parser Text
artist = string "#ARTIST"

bpm :: Parser Text
bpm = string "#BPM"

playlevel :: Parser Text
playlevel = string "#PLAYLEVEL"

rank :: Parser Text
rank = string "#RANK"

total :: Parser Text
total = string "#TOTAL"

wav :: Parser Text
wav = string "#WAV"

lnObj :: Parser Text
lnObj = string "#LNOBJ"

pHFData :: Parser Text -> Parser Text
pHFData p = p *> skipSpace *> takeTill isEndOfLine <* endOfLine

pLnObjDef :: Parser Text
pLnObjDef = lnObj *> char ':' *> takeTill isEndOfLine <* endOfLine

headerFieldStartStr :: String
headerFieldStartStr = "*---------------------- HEADER FIELD"

mainDataFieldStartStr :: String
mainDataFieldStartStr = "*---------------------- MAIN DATA FIELD"

divHeaderAndMain :: String -> (String, String)
divHeaderAndMain str = (unlines headerStrs, unlines mainDataStrs)
  where strs = lines str
        (headerStrs, mainDataStrs) = span (mainDataFieldStartStr /=) strs

convAllScr :: String -> String
convAllScr str = convAllScrStrHeader ++ (unlines notKey) ++ convAllScrStrMain
  where (headerStr, mainDataStr) = divHeaderAndMain str
        (key, notKey) = L.partition (\x -> Prelude.take 2 (Prelude.drop 4 x) `elem` (map unpack ksId)) $ lines mainDataStr
        convAllScrStrHeader = unlines $ convAllScrHeader $ lines headerStr
        convAllScrStrMain = convAllScrData $ getMainDataField (unlines key)

getMusicTitle :: String -> String
getMusicTitle str = drop 7 $ getMusicTitle' $ lines str
  where getMusicTitle' [] = ""
        getMusicTitle' (x:xs) | "#TITLE " `L.isPrefixOf` x = x
                              | otherwise = getMusicTitle' xs

getTotalValue :: String -> Int
getTotalValue str = read $ drop 7 $ getMusicTitle' $ lines str
  where getMusicTitle' [] = ""
        getMusicTitle' (x:xs) | "#TOTAL " `L.isPrefixOf` x = x
                              | otherwise = getMusicTitle' xs

-- HeaderDataField Process
convAllScrHeader :: [String] -> [String]
convAllScrHeader x = convAllScrHeader' x []
  where convAllScrHeader' [] ret = reverse ret
        convAllScrHeader' (x:xs) ret | "#TITLE " `L.isPrefixOf` x = convAllScrHeader' xs ((x++"_ALLSCR ver"):ret)
                                     | "#ARTIST " `L.isPrefixOf` x = convAllScrHeader' xs ((x++" / modified to ASCR_Tool"):ret)
                                     | otherwise = convAllScrHeader' xs (x:ret)

getLNOBJIDs :: String -> [WavId]
getLNOBJIDs str = getLNOBJID defLNOBJs []
  where defLNOBJs = filter (\x -> "#LNOBJ " `L.isPrefixOf` x) $ lines str
        getLNOBJID [] ret = ret
        getLNOBJID (x:xs) ret = getLNOBJID xs (T.pack (drop 7 x):ret)


-- MainDataField Process
getMainDataField :: String -> [MainDataField]
getMainDataField cont = case parseOnly pMFDatas (T.pack cont) of
  Left _ -> error "Parse Error."
  Right results -> results

pMFDatas :: Parser [MainDataField]
pMFDatas = do
--  string $ T.pack mainDataFieldStartStr
  many endOfLine
  many pMFData

pMFData :: Parser MainDataField
pMFData = do
  char '#'
  bar <- T.take 3
  kId <- T.take 2
  char ':'
  dat <- takeTill isEndOfLine
  many endOfLine
  let rhythm = convRhythm dat
      objs = convObjs dat
  return $ MainDataField bar kId rhythm objs

convRhythm :: Text -> Int
convRhythm x = (T.length x) `div` 2

convObjs :: Text -> [ObjInfo]
convObjs dat = filter (\(x, y) -> y /= "00") objsData
  where objData = 2 `chunksOf` dat
        objsData = L.zip [0..] objData

-- ObjCount Process
getCntObjKeys :: String -> (Int, Int, Int, Int, Int, Int, Int, Int)
getCntObjKeys str = getCntObjKeysTpl lnObjs mData
  where (headerStr, mainDataStr) = divHeaderAndMain str
        lnObjs = getLNOBJIDs headerStr
        mData = getMainDataField mainDataStr

cntObjKey :: [WavId] -> Text -> [MainDataField] -> Int
--cntObjKey lnObjs k mds = L.length (L.intersect keyObjs lnObjs)-- 数えるときにLNOBJのIDを抜いて計算
cntObjKey lnObjs k mds = L.length (filter (\x -> x `notElem` lnObjs) keyObjs)-- 数えるときにLNOBJのIDを抜いて計算
  where getKeyAndObj md = (getKId md, getObjs md)
        keyAndObjs = map getKeyAndObj mds
        keyObjs = map snd $ Prelude.concat $ map snd $ filter (\x -> k == fst x) keyAndObjs

getCntObjKeysTpl :: [WavId] -> [MainDataField] -> (Int, Int, Int, Int, Int, Int, Int, Int)
getCntObjKeysTpl lnObjs mds = (c1, c2, c3, c4, c5, c6, c7, cScr)
  where cntObjKeyRemLnObj = cntObjKey lnObjs
        cntObj1Key = cntObjKeyRemLnObj k1 mds 
        cntObjL1Key = (cntObjKeyRemLnObj lk1 mds) `div` 2
        c1 = cntObj1Key + cntObjL1Key
        cntObj2Key = cntObjKeyRemLnObj k2 mds 
        cntObjL2Key = (cntObjKeyRemLnObj lk2 mds) `div` 2
        c2 = cntObj2Key + cntObjL2Key
        cntObj3Key = cntObjKeyRemLnObj k3 mds 
        cntObjL3Key = (cntObjKeyRemLnObj lk3 mds) `div` 2
        c3 = cntObj3Key + cntObjL3Key
        cntObj4Key = cntObjKeyRemLnObj k4 mds 
        cntObjL4Key = (cntObjKeyRemLnObj lk4 mds) `div` 2
        c4 = cntObj4Key + cntObjL4Key
        cntObj5Key = cntObjKeyRemLnObj k5 mds 
        cntObjL5Key = (cntObjKeyRemLnObj lk5 mds) `div` 2
        c5 = cntObj5Key + cntObjL5Key
        cntObj6Key = cntObjKeyRemLnObj k6 mds 
        cntObjL6Key = (cntObjKeyRemLnObj lk6 mds) `div` 2
        c6 = cntObj6Key + cntObjL6Key
        cntObj7Key = cntObjKeyRemLnObj k7 mds 
        cntObjL7Key = (cntObjKeyRemLnObj lk7 mds) `div` 2
        c7 = cntObj7Key + cntObjL7Key
        cntObjScrKey = cntObjKeyRemLnObj scr mds 
        cntObjLScrKey = (cntObjKeyRemLnObj lscr mds) `div` 2
        cScr = cntObjScrKey + cntObjLScrKey



-- ALLSCR Process
type BarContentMap = M.Map Bar KeyLanes
type BarContents = [BarContent]
type BarContent = (Bar, KeyLanes)
type Bar = Text

type KeyLanes = M.Map KeyInfo [ObjInfo]
type KeyLane = (KeyInfo, [ObjInfo])

type KeyInfo = (KeyId, Rhythm)
type KeyId = Text
type Rhythm = Int


convMainDataFieldsToBmsData :: [MainDataField] -> String
convMainDataFieldsToBmsData mdData = convMainDataFieldsToBmsData' mdData []

convMainDataFieldsToBmsData' :: [MainDataField] -> String -> String
convMainDataFieldsToBmsData' [] r = r
convMainDataFieldsToBmsData' (x:xs) ret = convMainDataFieldsToBmsData' xs (objStr++ret)
  where objStr = convMainDataFieldToBMSData x

convMainDataFieldToBMSData :: MainDataField -> String
convMainDataFieldToBMSData x = ret
  where MainDataField bar kId rhythm objs = x
        ret = T.unpack $ T.concat ["#", bar, kId, ":", (getObjsStr rhythm objMap ""), T.pack ['\n']]
        objMap = M.fromList objs

getObjsStr :: Int -> M.Map Pos WavId -> Text -> Text
getObjsStr 0 _ r = r
getObjsStr n objMap ret = getObjsStr (n-1) objMap (objWavId `T.append` ret)
  where objWavId = M.findWithDefault "00" (n-1) objMap 

convAllScrData :: [MainDataField] -> String
convAllScrData md = convMainDataFieldsToBmsData $ L.filter (\x -> getObjs x /= []) $ convBarContentsToMDFileds bContents mdOther
  where (mdKeyScr, mdOther) = partitionObjAndOtherObj md 
        bContentMap = convMDFiledsToBarContents mdKeyScr
        bContents = convToAllscrDatas bContentMap

convMDFiledsToBarContents :: [MainDataField] -> BarContentMap
convMDFiledsToBarContents dat = convMDFiledsToBarContents' dat M.empty

convMDFiledsToBarContents' :: [MainDataField] -> BarContentMap -> BarContentMap
convMDFiledsToBarContents' [] r = r
convMDFiledsToBarContents' (x:xs) r = convMDFiledsToBarContents' xs ret
  where bar = getBar x
        kId = getKId x
        rhythm = getRhythm x
        objs = getObjs x
        keyLane = M.singleton (kId, rhythm) objs
        ret = M.insertWith (M.unionWith (++)) bar keyLane r

-- BarContentsからMainDataFieldに変換
convBarContentsToMDFileds :: BarContents -> [MainDataField] -> [MainDataField]
convBarContentsToMDFileds [] r = r
convBarContentsToMDFileds (x:xs) ret = convBarContentsToMDFileds xs result
  where (bar, keylanes) = x
        keyLaneList = M.toAscList keylanes
        result = convKeyLaneListToMainDataField bar keyLaneList ret

convKeyLaneListToMainDataField :: Bar -> [KeyLane] -> [MainDataField] -> [MainDataField]
convKeyLaneListToMainDataField b [] r = r
convKeyLaneListToMainDataField b (x:xs) ret = convKeyLaneListToMainDataField b xs (md:ret)
  where (keyInfo, objs) = x
        (keyId, rhythm) = keyInfo
        md = MainDataField b keyId rhythm objs

-- 鍵盤と皿、その他で分ける
partitionObjAndOtherObj :: [MainDataField] -> ([MainDataField], [MainDataField])
partitionObjAndOtherObj = L.partition (\x -> getKId x `elem` ksId) 

-- 全小節分全皿譜面に変換
convToAllscrDatas :: BarContentMap -> BarContents
convToAllscrDatas bcs = convToAllscrDatas' bcsList []
  where bcsList = M.toList bcs
        convToAllscrDatas' [] ret = ret
        convToAllscrDatas' (x:xs) ret = 
          convToAllscrDatas' xs ((convToAllscrData x):ret)

-- 一小節分全皿譜面に変換
convToAllscrData :: BarContent -> BarContent
convToAllscrData (bar, keyLanes) = (bar, convKeyLanes keyLanes)

-- 与えられた一小節分の譜面を全皿譜面に変換
-- オブジェを皿に移動させる
convKeyLanes :: KeyLanes -> KeyLanes
convKeyLanes kls = M.fromList (retScrLane:retOrgLane)
  where (scrLane, kLanes) = M.partitionWithKey (\(kId, _) _ -> kId == scr) kls
        scrRhythm = calcScrRhythm $ M.keys kls
        scrLaneInit = ((scr, scrRhythm), [])
        (_, addScrLane) = moveLaneObjToScr getScrLaneListHead scrLaneInit
        scrLaneList = (M.toList scrLane)
        getScrLaneListHead = if L.length scrLaneList == 0 then (("16",1), []) else head scrLaneList
        kLanesList = M.toDescList kLanes
        (retOrgLane, retScrLane) = moveLanesObjToScr kLanesList addScrLane ([], addScrLane)

-- 皿のレーンに移動させた後の譜面の状態が（鍵盤側, 皿側）で返ってくる --OK
moveLanesObjToScr :: [KeyLane] -> KeyLane -> ([KeyLane], KeyLane) -> ([KeyLane], KeyLane)
moveLanesObjToScr [] _ ret = ret
moveLanesObjToScr (orgx:orgxs) scrLane (retOrgL, retScrL) = moveLanesObjToScr orgxs rScrLane (resultOrgLane, rScrLane)
  where (rOrgLane, rScrLane) = moveLaneObjToScr orgx scrLane
        resultOrgLane = (rOrgLane:retOrgL)

-- 1レーン分のオブジェを皿に移動させる --OK
moveLaneObjToScr :: KeyLane -> KeyLane -> (KeyLane, KeyLane)
moveLaneObjToScr o@(orgKeyInfo, orgObjInfo) s@(scrKeyInfo, scrObjInfo) = (convOrg, convScr)
  where (convOrg, convScr) = moveObj o s ((orgKeyInfo, []), s)

-- レーン同士で鍵盤から皿へオブジェを移す　--OK
moveObj :: KeyLane -> KeyLane -> (KeyLane, KeyLane) -> (KeyLane, KeyLane)
moveObj (_, []) _ ret = ret
moveObj (kInfo, x:xs) s@(srcKInfo, srcOInfo) ((cOrgK, cOrgO), (cScrK, cScrO)) | canObjMove (kInfo, x) s = moveObj (kInfo, xs) (srcKInfo, cScrObj:srcOInfo) ((cOrgK, cOrgO), (cScrK, cScrObj:cScrO))
                                                                              | otherwise = moveObj (kInfo, xs) (srcKInfo, srcOInfo) ((cOrgK, x:cOrgO), (cScrK, cScrO))
  where rhythm1 = snd kInfo
        rhythm2 = snd srcKInfo
        commonRhythm = lcm rhythm1 rhythm2
        p1 = (commonRhythm `div` rhythm1) * (fst x)
        cScrObj = (p1, snd x)

-- 皿のリズムを求める --OK
calcScrRhythm :: [KeyInfo] -> Int
calcScrRhythm = (\x -> foldl lcm 1 (map snd x))

-- オブジェを移動させることが出来るか？ --OK
canObjMove :: (KeyInfo, ObjInfo) -> KeyLane -> Bool
canObjMove (kInfo1, oInfo) (kInfo2, osInfo) = p1 `notElem` ps2
  where rhythm1 = snd kInfo1
        rhythm2 = snd kInfo2
        commonRhythm = lcm rhythm1 rhythm2
        p1 = (commonRhythm `div` rhythm1) * (fst oInfo)
        ps2 = map ((commonRhythm `div` rhythm2) *) $ map fst osInfo
