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
import Dados


-- | Step1: Count how many times each item appears in all transactions
countItems transactions = mapReduceByKey (\x -> (x,1)) (+) transactions


-- | Step2: Eliminate items that do not appear in transactions enough.
applyThreshold transactionsLength xs = parfilter (\(x,y) -> y > minsup*transactionsLength) xs 


-- | Step3: Sort the list from most frequent item to the least one.
sortbyMostFrequent countItems = sortByKey countItems


-- | PRIVATE
-- | Sort a transaction from the most to least commom element.
sortTransaction headerTable transaction = [fst x | x <- headerTable, fst x `elem` transaction]


-- | Sort each transaction from most to least common element in header table.
sortTransactions transactions itemsCountAndSorted = parmap (sortTransaction itemsCountAndSorted) transactions