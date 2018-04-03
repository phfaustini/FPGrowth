module FPGrowth where

import FPTree

minsup = 0.4

insertTransaction :: [String] -> FPNode -> FPNode
insertTransaction transaction currentNode 
    | null transaction = currentNode
    | hasChild (head transaction) (fpchildren currentNode) = FPNode (fpitem currentNode) (fpcount currentNode) [insertTransaction (tail transaction) newElement]
    | otherwise = FPNode (fpitem currentNode) (fpcount currentNode) (fpchildren currentNode ++ [insertTransaction (tail transaction) newElement])
    where 
        inserted = insertNode (head transaction) currentNode
        newElement = head (fpchildren inserted)