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
  getMainDataField
) where

import Data.List as L (zip)
import Data.Text as T (Text, length, chunksOf, pack)
import Data.Attoparsec.Text as T
import Control.Applicative

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

data MainDataField = MainDataField {
  getKId :: Text,
  getBar :: Text,
  getRhythm :: Int,
  getObjs :: [Obj]
} deriving (Show)

type Pos = Int
type WavId = Text
type Obj = (Pos, WavId)

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

pMFDatas :: Parser [MainDataField]
pMFDatas = many pMFData

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
  return $ MainDataField kId bar rhythm objs

convRhythm :: Text -> Int
convRhythm x = (T.length x) `div` 2

convObjs :: Text -> [Obj]
convObjs dat = [x | x <- objsData, snd x /= "00"]
  where objData = 2 `chunksOf` dat
        objsData = L.zip [1..] objData

getMainDataField :: String -> [MainDataField]
getMainDataField cont = case parseOnly pMFDatas (T.pack cont) of
  Left _ -> error "Parse Error."
  Right results -> results

main :: IO ()
--main = print $ parseOnly (pHFData title) "#TITLE adfdsfadsf"
main = print $ parseOnly pMFData "#00201:0026002600260026\n"


