module FPGrowth where

import FPTree

f = FPNode {fpitem = "null", fpcount = 9, fpchildren = [FPNode {fpitem = "I2", fpcount = 7, fpchildren = [FPNode {fpitem = "I3", fpcount = 4, fpchildren = [FPNode {fpitem = "I1", fpcount = 2, fpchildren = [FPNode {fpitem = "I5", fpcount = 1, fpchildren = []}]},FPNode {fpitem = "I4", fpcount = 1, fpchildren = []},FPNode {fpitem = "I5", fpcount = 1, fpchildren = []}]},FPNode {fpitem = "I1", fpcount = 2, fpchildren = []},FPNode {fpitem = "I4", fpcount = 1, fpchildren = []}]},FPNode {fpitem = "I3", fpcount = 2, fpchildren = [FPNode {fpitem = "I1", fpcount = 2, fpchildren = []}]}]}

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
conditionalFPTree :: Num t => [(t, [String])] -> FPNode
conditionalFPTree cpb = buildFPTree transactionsReversedNotNull (FPNode "null" (length transactionsReversedNotNull) [])
    where
    transactions = map snd cpb
    transactionsReversed = map reverse transactions
    transactionsReversedNotNull = map (drop 1) transactionsReversed

buildConditionalFPTree :: Num t => [[(t, [String])]] -> [FPNode]
buildConditionalFPTree cpb = [ conditionalFPTree x | x <- cpb]
