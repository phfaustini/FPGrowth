module Main where

import System.Environment -- getArgs
import Control.Parallel
import TransactionsReader
import FPTree
import FPGrowth -- minsup
import qualified Data.Map as Map

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile -- ["4 3 1",    "12 3 1",    "23 34 9",   "2","90 1",    "5 1 12"]


main::IO ()
main = do
    args <- getArgs
    let filepath = head args
    fileContent <- readLines filepath -- "input/transactions.txt"
    let transactions = map words fileContent
    --print minsup
    --print transactions
    let itemsCounted = countItems transactions (Map.fromList [])
    --print itemsCounted -- MAP
    let itemsCountedAndPruned = applyThreshold (fromIntegral $ length transactions) itemsCounted  
    --print itemsCountedAndPruned -- MAP
    let itemsCountAndSorted = reverse $ sortbyMostFrequent itemsCounted
    print itemsCountAndSorted -- LIST
    putStrLn ""
    let itemsCountAndPrunedAndSorted = reverse $ sortbyMostFrequent itemsCountedAndPruned
    print itemsCountAndPrunedAndSorted -- LIST
    putStrLn ""
    let sortedTransactions = sortTransactions transactions itemsCountAndSorted []
    --print reverse sortedTransactions
    let sortedPrunedTransactions = sortTransactions transactions itemsCountAndPrunedAndSorted []
    print $ reverse sortedPrunedTransactions
    putStrLn ""
    let root = FPNode "null" (length transactions) []
    let fptree = buildFPTree (reverse sortedPrunedTransactions) root
    print fptree
    putStrLn ""
    let prunedFPTree = prune (minsup * fromIntegral (length transactions)) fptree
    print prunedFPTree