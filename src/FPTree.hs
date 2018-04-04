module FPTree where

data FPNode = FPNode { fpitem :: String, fpcount :: Int, fpchildren :: [FPNode]} deriving (Show, Eq)

hasChild :: String -> [FPNode] -> Bool
hasChild key children
    | null children = False
    | fpitem (head children) == key = True
    | otherwise = hasChild key (tail children)


incrementChild :: String -> [FPNode] -> [FPNode] -> [FPNode]
incrementChild key children output
    | null children = reverse output
    | fpitem (head children) == key = incrementChild key (tail children) (FPNode key (fpcount (head children) +1) (fpchildren (head children)) : output)
    | otherwise = incrementChild key (tail children) (head children:output)


insertNode :: String -> FPNode -> FPNode
insertNode key tree
    | hasChild key (fpchildren tree) = FPNode (fpitem tree) (fpcount tree) (incrementChild key (fpchildren tree) []) -- Tree has this child
    | otherwise = FPNode (fpitem tree) (fpcount tree) (FPNode key 1 [] : fpchildren tree) -- Add key to tree's children
