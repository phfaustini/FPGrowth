module Main where

import System.Environment -- getArgs
import Control.Parallel
import TransactionsReader
import FPTree
import FPGrowth -- minsup


readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile -- ["4 3 1",    "12 3 1",    "23 34 9",   "2","90 1",    "5 1 12"]


main::IO ()
main = do
    args <- getArgs
    let filepath = head args
    fileContent <- readLines filepath -- "input/transactions2.txt"
    let transactions = map words fileContent
    print transactions
    print minsup