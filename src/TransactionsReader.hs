module TransactionsReader where
    
-- | Count how many times each item appears in all transactions
{-
countItems transactions output
    | null transactions = output
    | otherwise = countItems (tail transactions) (countRow $ head transactions)
    where
        -- Se elemento nao esta em output, output[elemento] = 1, senao output[elemento] += 1
        countRow transaction
        | null transaction = 

-}
