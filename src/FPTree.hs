module FPTree where

minsup = 0.3 -- An item has to appear in at least xx% of all transactions

data FPNode = FPNode { fpitem :: String, fpcount :: Int, fpchildren :: [FPNode]} deriving (Show, Eq)

hasChild :: String -> [FPNode] -> Bool
hasChild key children
    | null children = False
    | fpitem (head children) == key = True
    | otherwise = hasChild key (tail children)


createBranch :: [String] -> FPNode
createBranch transaction
    | length transaction == 1 = FPNode (head transaction) 1 []
    | otherwise = FPNode (head transaction) 1 [createBranch (tail transaction)]

    
insertTransaction :: [String] -> FPNode -> FPNode
insertTransaction transaction root
    | toBeIncluded == 1 && incrementFPCountSomeChild = FPNode (fpitem root)
                                                        (fpcount root)
                                                        (head [FPNode (fpitem x) (fpcount x + 1) (fpchildren x) | x <- fpchildren root, fpitem x == head transaction] : otherChildren)
    | toBeIncluded > 1 && incrementFPCountSomeChild = FPNode (fpitem root)
                                                        (fpcount root)
                                                        (head [ insertTransaction (tail transaction) (FPNode (fpitem x) (fpcount x + 1) (fpchildren x)) | x <- fpchildren root, fpitem x == head transaction] : otherChildren)
    | otherwise = FPNode (fpitem root)
                            (fpcount root)
                            (createBranch transaction : fpchildren root)
    where 
        toBeIncluded = length transaction
        incrementFPCountSomeChild = hasChild (head transaction) (fpchildren root)
        otherChildren = [x | x <- fpchildren root, fpitem x /= head transaction]
    

buildFPTree :: [[String]] -> FPNode -> FPNode
buildFPTree transactions node
    | null transactions = node
    | otherwise = buildFPTree (tail transactions) (insertTransaction (head transactions) node)


-- | Remove branches whose fpcount is less than threshold
prune :: Double -> FPNode -> FPNode
prune threshold node
    | null $ fpchildren node = node
    | otherwise = FPNode (fpitem node) (fpcount node) [ prune threshold x | x <- fpchildren node, fromIntegral (fpcount x) >= threshold]