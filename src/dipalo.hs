{-# LANGUAGE OverloadedStrings, OverloadedLists, CPP #-}
  
import qualified Data.Vector as Vec
import Data.Vector((!))
import Data.Text hiding (map)
import Data.Monoid ((<>))
import System.IO
import qualified Data.List.Split as Split(chunksOf)
import Data.List (intercalate)
import qualified Data.Map as Map

data NumType = NumType {full :: Text, mid :: Text, final :: Text}

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)

nums :: Vec.Vector NumType
nums = Vec.fromList[
    NumType {full="lefela", mid="", final=""},
    NumType {full="nngwe", mid="", final="eng"}, 
    NumType {full="pedi", mid="ets", final="edi"}, 
    NumType {full="tharo", mid="ar", final="aro"}, 
    NumType {full="nne", mid="eny", final="ene"}, 
    NumType {full="hlano", mid="an", final="ano"}, 
    NumType {full="tshelela", mid="el", final="ela"}, 
    NumType {full="supa", mid="up", final="upa"}, 
    NumType {full="robedi", mid="ots", final="odi"}, 
    NumType {full="robong", mid="ony", final="ong"}, 
    NumType {full="leshome", mid="", final=""}
    ]
    
consts :: Map.Map Text Text
consts = Map.fromList[
    ("10", "leshome"), ("10-pre-outside", "lesh"), ("10-pre-inside", "esh"), ("10s-pre-outside", "mash"), ("10s-pre-inside", "ash"),
    ("100", "lekgolo"), ("100-pre-outside", "lekg"), ("100-pre-inside", "ekg"), ("100s-pre-outside", "makg"), ("100s-pre-inside", "akg")
    
    ]
    
#else

nums :: Vec.Vector NumType
nums = Vec.fromList[
    NumType {full="lēféla", mid="", final=""},
    NumType {full="ńńgwē", mid="", final="ḗng"}, 
    NumType {full="pedí", mid="ets", final="edí"}, 
    NumType {full="thárō", mid="ár", final="árō"}, 
    NumType {full="ńne", mid="ḗny", final="ḗne"}, 
    NumType {full="hlánō", mid="án", final="ánō"}, 
    NumType {full="tshḗlḗla", mid="ḗl", final="ḗla"}, 
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
    
#endif
    
group :: Int -> Text
group x
    | x == 10  = (consts Map.! "10")
    | x == 100 = (consts Map.! "100")
    | x < 10   = unit False x
    | x < 100  = ten False x
    | x < 1000 = hundred False x

unit :: Bool -> Int -> Text
unit False x = full . (nums !) $ x
unit  True x = final . (nums !) $ x

ten :: Bool -> Int -> Text
ten False x =
    if x `div` 10 == 1
        then (consts Map.! "10-pre-outside") <> unit True (x `mod` 10)
        else
            (consts Map.! "10s-pre-outside") <>
                if x `mod` 10 == 0
                    then final . (nums !) . (`div` 10) $ x
                    else (mid . (nums !) . (`div` 10) $ x) <> unit True (x `mod` 10)
ten True x
    | x `div` 10 == 1 = (consts Map.! "10-pre-inside") <> unit True (x `mod` 10)
    | x `mod` 10 == 0 = (consts Map.! "10s-pre-inside") <> (final . (nums !) . (`div` 10) $ x)
    |otherwise        = (mid . (nums !) . (`div` 10) $ x) <> unit True (x `mod` 10)

hundred :: Bool -> Int -> Text
hundred False x =
    if x `div` 100 == 1
        then "lēkg" <> (ten True ((`mod` 100) $ x))
        else
            "makg" <>
                if x `mod` 100 == 0
                    then (final . (nums !) . (`div` 100) $ x)
                    else (mid . (nums !) . (`div` 100) $ x) <> (ten True ((`mod` 100) $ x))
hundred True x =
    if x `div` 100 == 1
        then "ēkg" <> (unit True ((`mod` 100) $ x))
        else
            "akg" <>
                if x `mod` 100 == 0
                    then (final . (nums !) . (`div` 100) $ x)
                    else (mid . (nums !) . (`div` 100) $ x) <> (ten True ((`mod` 100) $ x))

l_transform :: Text -> Text
l_transform  = replace "lu" "du"

main = do
    hSetBuffering stdout NoBuffering

    putStr "Tlanya: "
    
    x <- getLine
    let xn = (read x) :: Int
     
    if xn < 0
        then 
            putStrLn "Bye!"
        else do
            --putStrLn . unpack . Main.group $ xn
            putStrLn . Data.List.intercalate ", " . Prelude.reverse .  map (unpack . l_transform .  Main.group . read . Prelude.reverse) . Split.chunksOf 3 . Prelude.reverse . show $ xn
            main

