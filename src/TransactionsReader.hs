module TransactionsReader where
    
import qualified Data.Map as Map

fromJust Nothing  = 0
fromJust (Just x) = x

-- | Count how many times each item appears in all transactions
countItems transactions counting
    | null transactions = counting
    | otherwise = countItems (tail transactions) (updateCounting (head transactions) counting)
    where
        -- Se elemento nao esta em counting, counting[elemento] = 1, senao counting[elemento] += 1
        updateCounting transaction counting
            | null transaction = counting
            | otherwise = updateCounting (tail transaction) (updateElement (head transaction) counting)
            where
                updateElement element counting = Map.insert element (check element counting + 1) counting
                check element counting
                        | Map.lookup element counting == Nothing = 0
                        | otherwise = fromJust (Map.lookup element counting)



