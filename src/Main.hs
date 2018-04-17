{- |
Module      :  Main.hs
Description :  Entry point and IO functions.
Copyright   :  Copyright (c) 2018 Pedro Faustini
License     :  See LICENSE

Maintainer  :  pedro.faustini@ufabc.edu.br
Stability   :  experimental
Portability :  non-portable (Tested only in Linux)

This module is the Main module, the entry point of the program.
All impure actions, like IO, are contained here, and only here.
-}

module Main where

import System.Environment -- getArgs
import Control.Parallel
import TransactionsHandler
import FPTree -- minsup
import FPGrowth
import qualified Data.Map as Map
import Data.List -- intercalate

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile


printFPTree :: FPNode -> String -> String
printFPTree node spaces
    | not (null (fpchildren node)) = "\n"++spaces ++ fpitem node ++ " : " ++ show(fpcount node) ++ "  " ++ intercalate (spaces++"    ") [printFPTree x (spaces++"    ") | x <- fpchildren node ]
    | otherwise = "\n"++spaces ++ fpitem node ++ " : " ++show(fpcount node) ++ "  []"


main::IO ()
main = do
    args <- getArgs
    let filepath = head args
    fileContent <- readLines filepath -- "input/transactions.txt"
    
    {-
        Step 1: Preprocessing.
        Database is read.
        TransactionsHandler functions now count items from transactions.
        Then, the headerTable is built.
        Last, infrequent items are pruned from the sorted transactions.
    -}
    let transactions = map words fileContent
    let itemsCounted = countItems transactions -- itemsCounted is like [("I1",6),("I2",7),("I3",6),("I4",2),("I5",2)]
    let itemsCountedAndPruned = applyThreshold (fromIntegral (length transactions)) itemsCounted  
    let headerTablePruned = reverse $ sortbyMostFrequent itemsCountedAndPruned
    putStr "HeaderTable pruned: "
    print headerTablePruned
    putStrLn ""
    let sortedPrunedTransactions = sortTransactions transactions headerTablePruned []
    

    {-
        Step 2: build FPTree
    -}
    let root = FPNode "null" (length transactions) []
    let fptree = buildFPTree (reverse sortedPrunedTransactions) root
    putStr (printFPTree fptree " ")
    putStrLn "\n"


    {-
        Step 3: FPGrowth
        Conditional pattern bases are extracted from FPTree, one base for each frequent item.
        Frequent item sets are then mined.
    -}
    let headerTablePrunedfromMintoMax = reverse headerTablePruned
    let cpbs = buildConditionalPatternBase headerTablePrunedfromMintoMax fptree
    let frequentSetsItems = frequentPatternItems (rawFrequentPatternItems cpbs []) (ceiling (minsup * fromIntegral (length transactions)))
    putStr "Frequent sets of items: "
    print frequentSetsItems
    putStrLn ""