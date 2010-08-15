{-# LANGUAGE QuasiQuotes #-}
{- Code to generate additional instances
-}
import Data.List 
import Data.Char
import Here

mkTuplize nm (mkType, mkValue) depth
  = unwords ["instance", nm, mkType depth, tuple, "where\n  ", map toLower nm, mkValue depth, "=", tuple]
  where tuple = mkTuple depth

mkUntuplize (mkType, mkValue) depth
  = unwords ["instance Untuple", tuple, mkType depth, "where\n  untuple", tuple, "=", mkValue depth]
  where tuple = mkTuple depth

tuplizeLvPairsList = mkTuplize "LTuple" (lvTypesList, lvValuesList)
tuplizeHList = mkTuplize "Tuple" (hTypesList, hValuesList)

untuplizeHList = mkUntuplize (hTypesList, hValuesList)

tuplizeHLists n = map tuplizeHList [1..n]
untuplizeHLists n = map untuplizeHList [2..n] -- can't define 1, because all other overlap and break fundeps
tuplizeLvPairsLists n = map tuplizeLvPairsList [1..n]

lvTypesList n = foldr1 mkHCons $ lvTypes n ++ ["HNil"]
lvValuesList n = foldr1 mkHCons $ lvValues n ++ ["HNil"]

hTypesList n = foldr1 mkHCons $ hTypes n ++ ["HNil"]
hValuesList n = foldr1 mkHCons $ hValues n ++ ["HNil"]


mkHCons x y = "(HCons " ++ x ++ " " ++ y ++ ")"

lvType n = "(LVPair l" ++ n' ++ " v" ++ n' ++ ")"
  where n' = show n
lvValue n = "(LVPair v" ++ n' ++ ")"
  where n' = show n

lvTypes n = map lvType [1..n]
lvValues n = map lvValue [1..n]

hType n = "v" ++ show n 
hValue n = "v" ++ show n 
hTypes n = map hType [1..n]
hValues n = map hValue [1..n]

mkTuple depth = wrap $ intercalate "," ["v" ++ show n | n <- [1..depth]]
  where wrap s = "(" ++ s ++ ")"

header = tail $ lines [$here|
{-# LANGUAGE MultiParamTypeClasses
           , FunctionalDependencies
           , FlexibleInstances
           #-}

module Data.HList.Tuple where

import Data.HList 

class Tuple a b | a -> b where
  tuple :: a -> b

class Untuple a b | a -> b where
  untuple :: a -> b

class LTuple a b | a -> b where
  ltuple :: a -> b
|]

tupleMod n = intercalate [""] [header, tuplizeHLists n, untuplizeHLists n, tuplizeLvPairsLists n] 
main = putStrLn . unlines $ tupleMod 50
