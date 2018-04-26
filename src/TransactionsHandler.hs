{- |
Module      :  TransactionsHandler.hs
Description :  Functions to deal with transactions.
Copyright   :  Copyright (c) 2018 Pedro Faustini
License     :  See LICENSE

Maintainer  :  pedro.faustini@ufabc.edu.br
Stability   :  experimental
Portability :  non-portable (Tested only in Linux)

This module contains functions to deal with transaction items.
The transactions are collected in Main.hs (through IO operations)
and processed here.
-}

module TransactionsHandler
(
    countItems,
    applyThreshold,
    sortbyMostFrequent,
    sortTransactions
)
where

import FPTree -- minsup, numberChunks
import Data.List -- sortBy
import Data.Ord -- comparing
import Dados
import Control.Parallel.Strategies


-- | Step1: Count how many times each item appears in all transactions
countItems transactions = mapReduceByKey (\x -> (x,1)) (+) transactions


-- | Step2: Eliminate items that do not appear in transactions enough.
applyThreshold transactionsLength xs = filter (\(x,y) -> y > minsup*transactionsLength) xs `using` parListChunk numberChunks rdeepseq


-- | Step3: Sort the list from most frequent item to the least one.
--sortbyMostFrequent :: Ord a => Map.Map a1 a -> [(a1, a)]
sortbyMostFrequent countItems = Data.List.sortBy (Data.Ord.comparing snd) countItems `using` parListChunk numberChunks rdeepseq


-- | PRIVATE
-- | Sort a transaction from the most to least commom element.
sortTransaction transaction headerTable = [fst x | x <- headerTable, fst x `elem` transaction]


-- | Sort each transaction from most to least common element in header table.
sortTransactions transactions itemsCountAndSorted = concat ([[sortTransaction x itemsCountAndSorted | x <- transactions]]) `using` parListChunk numberChunks rdeepseq
