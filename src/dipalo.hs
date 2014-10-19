{-# LANGUAGE OverloadedStrings, OverloadedLists, CPP #-}

import qualified Data.Vector as Vec
import Data.Vector((!))
import Data.Text hiding (map)
import Data.Monoid ((<>))
import System.IO
import qualified Data.List.Split as Split(chunksOf)
import System.Console.Haskeline
import qualified Data.Map as Map
import Data.Char (digitToInt)
import Data.Maybe (fromJust)
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import qualified Data.Text.ICU.Translit as Translit
#endif

data NumType = NumType {full :: Text, mid :: Text, final :: Text}

nums :: Vec.Vector NumType
nums = Vec.fromList[
    NumType {full="lēféla", mid="", final=""},
    NumType {full="ńńgwē", mid="", final="ḗng"},
    NumType {full="pedí", mid="ets", final="edí"},
    NumType {full="thárō", mid="ár", final="árō"},
    NumType {full="ńne", mid="ḗny", final="ḗne"},
    NumType {full="hlánō", mid="án", final="ánō"},
    NumType {full="tshḗléla", mid="ḗl", final="ḗla"},
    NumType {full="supá", mid="up", final="upá"},
    NumType {full="robedí", mid="ots", final="odí"},
    NumType {full="robṓng", mid="ṓny", final="ṓng"},
    NumType {full="lēshṓme", mid="", final=""}
    ]

consts :: Map.Map Text Text
consts = Map.fromList[
    ("10", "lēshṓme"), ("10-pre-outside", "lēsh"), ("10-pre-inside", "ēsh"), ("10s-pre-outside", "mash"), ("10s-pre-inside", "ash"),
    ("100", "lēkgṓlō"), ("100-pre-outside", "lēkg"), ("100s-pre-outside", "makg"), ("100-pre-inside", "ēkg"), ("100s-pre-inside", "akg")

    ]

group :: Bool -> Integer -> Text
group inside x
    | x == 10    = consts Map.! "10"
    | x == 100   = consts Map.! "100"
    | x < 10    = unit inside x
    | x < 100   = ten inside x
    | x < 1000  = hundred inside x
    | otherwise = undefined -- get compiler to shut up about non-exhaustive patterns

unit :: Bool -> Integer -> Text
unit False x = full . (nums !) . fromIntegral $  x
unit True x = final . (nums !) . fromIntegral $ x

ten :: Bool -> Integer -> Text
ten False x =
    if x `div` 10 == 1
        then (consts Map.! "10-pre-outside") <> unit True (x `mod` 10)
        else
            (consts Map.! "10s-pre-outside") <>
                if x `mod` 10 == 0
                    then final . (nums !) . fromIntegral . (`div` 10) $ x
                    else (mid . (nums !) . fromIntegral . (`div` 10) $ x) <> unit True (x `mod` 10)
ten True x
    | x `div` 10 == 1 = (consts Map.! "10-pre-inside") <> unit True (x `mod` 10)
    | x `mod` 10 == 0 = (consts Map.! "10s-pre-inside") <> (final . (nums !) . fromIntegral . (`div` 10) $ x)
    |otherwise        = (mid . (nums !) . fromIntegral . (`div` 10) $ x) <> unit True (x `mod` 10)

hundred :: Bool -> Integer -> Text
hundred False x =
    if x `div` 100 == 1
        then "lēkg" <> (ten True ((`mod` 100) $ x))
        else
            "makg" <>
                if x `mod` 100 == 0
                    then (final . (nums !) . fromIntegral . (`div` 100) $ x)
                    else (mid . (nums !) . fromIntegral . (`div` 100) $ x) <> (ten True ((`mod` 100) $ x))
hundred True x =
    if x `div` 100 == 1
        then "ēkg" <> (unit True ((`mod` 100) $ x))
        else
            "akg" <>
                if x `mod` 100 == 0
                    then (final . (nums !) . fromIntegral . (`div` 100) $ x)
                    else (mid . (nums !) . fromIntegral . (`div` 100) $ x) <> (ten True ((`mod` 100) $ x))

l_transform :: Text -> Text
l_transform  = replace "lu" "du"

data ThouGroup = ThouGroup {_TGposition :: Integer, _TGgroup :: Integer} deriving Show

groupThousands :: Integer -> [ThouGroup]
groupThousands = tagGroups . map (toInteger . digitToInt) . show

tagGroups :: [Integer] -> [ThouGroup]
tagGroups = Prelude.reverse . map (\(n, xs) -> ThouGroup {_TGposition = n, _TGgroup = Prelude.foldl ((+).(*10)) 0 xs}) . Prelude.zip [0::Integer ..] . map Prelude.reverse . Split.chunksOf 3 . Prelude.reverse

thouGroupToText :: ThouGroup -> Text
thouGroupToText tg =
  if _TGgroup tg == 0 then "" else
    case _TGposition tg of
       0  -> gftg
       _  ->
         (if _TGgroup tg == 1 then "sē" else "di") <>
         case _TGposition tg of
              1 -> "kētē" <> if _TGgroup tg == 1 then "" else " tsé " <> gftg
              _ -> "k" <> if _TGgroup tg == 1 then "" else (Main.group True $ _TGposition tg) <> " tsé " <> gftg
  where
    gftg = Main.group False $ _TGgroup tg

payload :: Integer -> Text
payload =  Data.Text.intercalate " lē " . Prelude.filter (/= "") . map thouGroupToText . groupThousands

removeDiacr :: Text -> Text
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
removeDiacr = Translit.transliterate (Translit.trans "NFD; [:M:] Remove; NFC") --whether or not the ICU libraries can be installed on Windows is another issue entirely...
#else
removeDiacr = id
#endif

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop = do
    --hSetBuffering stdout NoBuffering
      inp <- getInputLine "Tlanya: "
      case inp of
        Nothing    -> return ()
        Just x -> do
          let n = (read x) :: Integer
          if n < 0
            then
              outputStrLn "Bye!"
            else do
              outputStrLn . unpack . payload $ n
              loop

