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


module FPGrowth 
(
    buildConditionalPatternBase,
    frequentPatternItems
)
where

import FPTree
import Data.List -- subsequences
import Control.Parallel
import Control.Parallel.Strategies
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
subCPBtoKeyValue subcpb = map (\x -> (head (snd subcpb) : x, fst subcpb)) (subsequences (init (drop 1 (snd subcpb))))

-- | PRIVATE
concatsubcpbs cpb = concat [subCPBtoKeyValue sub | sub <- cpb]

-- | PRIVATE
frequentPatternCPB threshold cpb = filter (\x -> snd x >= threshold) $ combine (+) $ concatsubcpbs cpb

frequentPatternItems cpbs threshold = [ frequentPatternCPB threshold cpb | cpb <- cpbs ] `using` parListChunk 4 rdeepseq

