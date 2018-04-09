module FPGrowth where

import FPTree

f = FPNode {fpitem = "null", fpcount = 9, fpchildren = [FPNode {fpitem = "I2", fpcount = 7, fpchildren = [FPNode {fpitem = "I3", fpcount = 4, fpchildren = [FPNode {fpitem = "I1", fpcount = 2, fpchildren = [FPNode {fpitem = "I5", fpcount = 1, fpchildren = []}]},FPNode {fpitem = "I4", fpcount = 1, fpchildren = []},FPNode {fpitem = "I5", fpcount = 1, fpchildren = []}]},FPNode {fpitem = "I1", fpcount = 2, fpchildren = []},FPNode {fpitem = "I4", fpcount = 1, fpchildren = []}]},FPNode {fpitem = "I3", fpcount = 2, fpchildren = [FPNode {fpitem = "I1", fpcount = 2, fpchildren = []}]}]}

conditionalPatternBase :: String -> FPNode -> [String] -> [[String]]
conditionalPatternBase key node path 
    | fpitem node == key = [key : path]
    | null $ fpchildren node = []
    | not (null recursiveCall) = concatCPB recursiveCall
    | otherwise = []
    where
        recursiveCall = [conditionalPatternBase key x (fpitem node : path) | x <- fpchildren node ]

        
concatCPB :: [[[String]]] -> [[String]]
concatCPB l
    | null l = []
    | otherwise = head l ++ concatCPB (tail l)



{-depthSearchFirstBranch :: String -> FPNode -> [String] -> [String]
depthSearchFirstBranch key node path
    | fpitem node == key = key : path
    | null $ fpchildren node = []
    | otherwise = depthSearch key (head (fpchildren node)) (fpitem node : path)

pruneFirstLeaf node 
    | null $ fpchildren node = node
    | otherwise = FPNode (fpitem node) (fpcount node) [ pruneElement key x | x <- fpchildren node, fpitem x /= key]
-}