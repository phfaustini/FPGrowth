module TransactionsReader where

import FPGrowth
import qualified Data.Map as Map
import Data.List
import Data.Ord

fromJust Nothing  = 0
fromJust (Just x) = x

-- | Step1: Count how many times each item appears in all transactions
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
                        | Map.lookup element counting == Nothing = 0
                        | otherwise = fromJust (Map.lookup element counting)


-- | How many items there are in all transactions
getNumberElements transactions counter
    | null transactions = counter
    | otherwise = getNumberElements (tail transactions) counter + length (head transactions)


-- | Step2: Eliminate items that do not appear in transactions enough.
applyThreshold totalTransactions = Map.filter (>= minsup*totalTransactions)

-- | Step3: Sort the list from most frequent item to the least one.
sortbyMostFrequent countItems = Data.List.sortBy (Data.Ord.comparing snd) (Map.toList countItems)