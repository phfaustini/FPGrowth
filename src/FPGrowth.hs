module FPGrowth where

import FPTree

minsup = 0.4

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
