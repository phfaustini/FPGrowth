{- |
Module      :  TransactionsHandler.hs
Description :  Functions to deal with transactions.
Copyright   :  Copyright (c) 2018 Pedro Faustini
License     :  See LICENSE

Maintainer  :  pedro.faustini@ufabc.edu.br
Stability   :  stable
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

import FPTree
import qualified Data.Map as Map
import Data.Maybe
import Data.List
import Data.Ord


-- | Step1: Count how many times each item appears in all transactions
countItems :: (Eq a, Num a, Ord k) => [[k]] -> Map.Map k a -> Map.Map k a
countItems transactions counting
    | null transactions = counting
    | otherwise = countItems (tail transactions) (updateCounting (head transactions) counting)
    where
        updateCounting transaction counting
            | null transaction = counting
            | otherwise = updateCounting (tail transaction) (updateElement (head transaction) counting)
            where
                updateElement element counting = Map.insert element (check element counting + 1) counting
                check element counting
                        | isNothing (Map.lookup element counting) = 0
                        | otherwise = fromJust (Map.lookup element counting)


-- | Step2: Eliminate items that do not appear in transactions enough.
applyThreshold :: Double -> Map.Map k Double -> Map.Map k Double
applyThreshold transactionsLength = Map.filter (>= minsup*transactionsLength)

-- | Step3: Sort the list from most frequent item to the least one.
sortbyMostFrequent :: Ord a => Map.Map a1 a -> [(a1, a)]
sortbyMostFrequent countItems = Data.List.sortBy (Data.Ord.comparing snd) (Map.toList countItems)

-- | PRIVATE
-- | Sort a transaction from the most to least commom element.
sortTransaction transaction itemsCountAndSorted output -- itemsCountAndSorted is LIST
    | null itemsCountAndSorted = reverse output
    | target `elem` transaction = sortTransaction transaction (tail itemsCountAndSorted) (target:output)
    | otherwise = sortTransaction transaction (tail itemsCountAndSorted) output
    where
        target = fst $ head itemsCountAndSorted

sortTransactions transactions itemsCountAndSorted output -- itemsCountAndSorted is LIST
    | null transactions = output
    | otherwise = sortTransactions (tail transactions) itemsCountAndSorted output ++ [sortTransaction (head transactions) itemsCountAndSorted []]