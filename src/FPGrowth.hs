{- |
Module      :  FPGrowth.hs
Description :  Frequent Pattern items mining
Copyright   :  Copyright (c) 2018 Pedro Faustini
License     :  See LICENSE

Maintainer  :  pedro.faustini@ufabc.edu.br
Stability   :  experimental
Portability :  non-portable (Tested only in Linux)

This module contains functions to retrieve frequent items from a FPTree.
-}


module FPGrowth where

import FPTree
import Data.List -- subsequences
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

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

-- | PRIVATE
conditionalFPTree :: (Eq a, Num a) => [(a, [String])] -> [(String, Double)] -> FPNode
conditionalFPTree cpb headerTable = buildFPTree transactionsReversedNotNull (FPNode key (length transactionsReversedNotNull) []) -- prune??
    where
    transactions = replicateLists cpb -- map snd cpb
    transactionsReversed = map reverse transactions
    removeFirstLast l = init (drop 1 l)
    transactionsReversedNotNull = filter (/= []) (map removeFirstLast transactionsReversed) -- "null" and element itself removed. Empties filtered out.
    key = head (snd (head cpb))
    t = findThreshold key headerTable


-- | PRIVATE 
findThreshold :: String -> [(String, Double)] -> Double
findThreshold key headerTable
    | null headerTable = 0
    | fst (head headerTable) == key = snd (head headerTable)
    | otherwise = findThreshold key (tail headerTable)

buildConditionalFPTree :: (Eq a, Num a) => [[(a, [String])]] -> [(String, Double)] -> [FPNode]
buildConditionalFPTree cpbs headerTable = [ conditionalFPTree x headerTable | x <- cpbs]

-- | PRIVATE
replicateList :: (Eq a, Num a) => (a, b) -> [b]
replicateList pair 
    | fst pair == 1 = [snd pair]
    | otherwise = snd pair : replicateList (fst pair-1, snd pair)

-- | PRIVATE
replicateLists :: (Eq a, Num a) => [(a, t)] -> [t]
replicateLists pairs
    | null pairs = []
    | otherwise = replicateList (head pairs) ++ replicateLists (tail pairs)


-- | PRIVATE
patternsCombination :: [(t, [String])] -> [(t, [[String]])] -> [(t, [[String]])]
patternsCombination cpb output
    | null cpb = output
    | otherwise = patternsCombination (tail cpb) ((count, combinations) : output)
    where
        count = fst $ head cpb
        items = init (drop 1 (snd $ head cpb))
        combinations = map (\x -> item : x) $ filter (/= []) $ subsequences items
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


-- | PRIVATE
reduceCombinations :: (Num a1, Ord a) => [(a1, [a])] -> Map a a1 -> Map a a1
reduceCombinations cpb myMap
    | null cpb = myMap
    | otherwise = reduceCombinations (tail cpb) (reduceCombination (head cpb) myMap)



rawFrequentPatternItems :: Num a1 => [[(a1, [String])]] -> [Map [String] a1] -> [Map [String] a1]
rawFrequentPatternItems cpbs frequentitems
    | null cpbs = frequentitems
    | otherwise = rawFrequentPatternItems (tail cpbs) (reduceCombinations cpb Map.empty : frequentitems)
    where
        cpb = patternsCombination (head cpbs) []


frequentPatternItems :: Ord a => [Map a1 a] -> a -> [(a1, a)]
frequentPatternItems frequentitemsmap minsup = [y | x<-frequentitemsmap, y <- Map.toList x, snd y >= minsup  ]
