{- |
Module      :  FPGrowth.hs
Description :  Frequent Pattern items mining
Copyright   :  Copyright (c) 2018 Pedro Faustini
License     :  See LICENSE

Maintainer  :  pedro.faustini@ufabc.edu.br
Stability   :  stable
Portability :  non-portable (Tested only in Linux)

This module contains functions to retrieve frequent items from a FPTree.
-}


module FPGrowth 
(
    buildConditionalPatternBase,
    rawFrequentPatternItems,
    frequentPatternItems
)
where

import FPTree
import Data.List -- subsequences
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Dados

-- | PRIVATE
rawConditionalPatternBase :: String -> FPNode -> [String] -> [[String]]
rawConditionalPatternBase key node path 
    | fpitem node == key = [show (fpcount node)] : [key : path]
    | null $ fpchildren node = []
    | not (null rawCecursiveCall) = concatCPB rawCecursiveCall
    | otherwise = []
    where
        rawCecursiveCall = [rawConditionalPatternBase key x (fpitem node : path) | x <- fpchildren node ]

-- | PRIVATE
concatCPB :: [[[String]]] -> [[String]]
concatCPB l
    | null l = []
    | otherwise = head l ++ concatCPB (tail l)

-- | PRIVATE
conditionalPatternBase :: [[String]] -> [(Integer, [String])] -> [(Integer, [String])]
conditionalPatternBase raw output
    | null raw = output
    | otherwise = conditionalPatternBase (tail (tail raw))  (( read (head (head raw)) :: Integer, head (tail raw) ) : output)

-- | PRIVATE
headerTableToConditionalPatternBase :: [(String, b)] -> FPNode -> [(Integer, [String])]
headerTableToConditionalPatternBase headerTable node
    | null headerTable = []
    | otherwise = conditionalPatternBase (rawConditionalPatternBase (fst (head headerTable)) node []) []

buildConditionalPatternBase :: [(String, b)] -> FPNode -> [[(Integer, [String])]]
buildConditionalPatternBase headerTable node
    | null headerTable = []
    | otherwise = headerTableToConditionalPatternBase headerTable node : buildConditionalPatternBase (tail headerTable) node

-- -------------------------------------------------------------------------------------------------------------------------------
    
-- | PRIVATE
patternsCombination :: [(t, [String])] -> [(t, [[String]])] -> [(t, [[String]])]
patternsCombination cpb output
    | null cpb = output
    | otherwise = patternsCombination (tail cpb) ((count, combinations) : output)
    where
        count = fst $ head cpb
        items = init (drop 1 (snd $ head cpb))
        combinations = parmap (\x -> item : x) $ filter (/= []) $ subsequences items
        item = head $ snd $ head cpb


-- | PRIVATE
reduceCombination :: (Num a1, Ord a) => (a1, [a]) -> Map a a1 -> Map a a1
reduceCombination subcpb myMap
    | null $ snd subcpb = myMap
    | otherwise = reduceCombination (fst subcpb, tail (snd subcpb)) updateMap
    where 
        key = head $ snd subcpb
        updateMap = Map.insert key updatedValue myMap
        updatedValue
            | isNothing (Map.lookup key myMap) = fst subcpb
            | otherwise = fromJust (Map.lookup key myMap) + fst subcpb


-- | PRIVATE. Can parallelise, calling reduceCombination in parallel for each subcpb
reduceCombinations :: (Num a1, Ord a) => [(a1, [a])] -> Map a a1 -> Map a a1
reduceCombinations cpb myMap
    | null cpb = myMap
    | otherwise = reduceCombinations (tail cpb) (reduceCombination (head cpb) myMap)


-- | It mines ALL pattern items, regardless of minimum support.
rawFrequentPatternItems :: Num a1 => [[(a1, [String])]] -> [Map [String] a1] -> [Map [String] a1]
rawFrequentPatternItems cpbs frequentitems
    | null cpbs = frequentitems
    | otherwise = rawFrequentPatternItems (tail cpbs) (reduceCombinations cpb Map.empty : frequentitems)
    where
        cpb = patternsCombination (head cpbs) []


-- | It extracts only the FREQUENT pattern items, according to minimum support. 
frequentPatternItems :: Ord a => [Map a1 a] -> a -> [(a1, a)]
frequentPatternItems [] _ = []
frequentPatternItems frequentitemsmap minsup = [y | x<-frequentitemsmap, y <- Map.toList x, snd y >= minsup  ]
