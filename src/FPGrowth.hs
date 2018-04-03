module FPGrowth where

import FPTree

minsup = 0.4


{-
insertTransaction :: [String] -> FPNode -> FPNode -> FPNode
insertTransaction transaction currentNode outpuTtree
    | null transaction = outpuTtree
    | otherwise = insertTransaction (tail transaction) (head (fpchildren inserted)) (FPNode (fpitem outpuTtree) (fpcount outpuTtree) (fpchildren inserted))
    where
        inserted = insertNode (head transaction) currentNode
-}

insertTransaction :: [String] -> FPNode -> FPNode
insertTransaction transaction currentNode 
    | null transaction = currentNode
    | otherwise = FPNode (fpitem currentNode) (fpcount currentNode) [insertTransaction (tail transaction) newElement]
    where 
        inserted = insertNode (head transaction) currentNode
        newElement = head (fpchildren inserted)