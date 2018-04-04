module FPGrowth where

import FPTree

minsup = 0.3 -- An item has to appear in at least 40% of all transactions

insertTransaction :: [String] -> FPNode -> FPNode
insertTransaction transaction currentNode 
    | null transaction = currentNode
    | hasChild (head transaction) oldChildren = FPNode
                                                    (fpitem currentNode) 
                                                    (fpcount currentNode)
                                                    (removeOldElement oldChildren newElement ++ [insertTransaction (tail transaction) newElement])
    | otherwise = FPNode 
                        (fpitem currentNode) 
                        (fpcount currentNode) 
                        (oldChildren ++ [insertTransaction (tail transaction) newElement])
    where 
        inserted = insertNode (head transaction) currentNode
        newElement = head (fpchildren inserted)
        oldChildren = fpchildren currentNode
        removeOldElement oldChildren newElement = [x | x <- oldChildren, fpitem x /= fpitem newElement ]


buildFPTree :: [[String]] -> FPNode -> FPNode
buildFPTree transactions node
    | null transactions = node
    | otherwise = buildFPTree (tail transactions) (insertTransaction (head transactions) node)


-- | Remove branches whose fpcount is less than threshold
prune :: Double -> FPNode -> FPNode
prune threshold node
    | null $ fpchildren node = node
    | otherwise = FPNode (fpitem node) (fpcount node) [ prune threshold x | x <- fpchildren node, fromIntegral (fpcount x) >= threshold]