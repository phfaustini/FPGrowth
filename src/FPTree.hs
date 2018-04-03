module FPTree where

data FPNode = FPNode { fpitem :: String, fpcount :: Integer, fpchildren :: [FPNode]} deriving (Show, Eq)

root = FPNode "null" (-1) []
e = FPNode "e" 1 []
d = FPNode "d" 1 [e]
c = FPNode "c" 1 [d]
b = FPNode "b" 1 []
a = FPNode "a" 1 [b,c]


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
    -- | key == "null" = FPNode "null" (-1) [] -- Tree is created
    | hasChild key (fpchildren tree) = FPNode (fpitem tree) (fpcount tree) (incrementChild key (fpchildren tree) []) -- Tree has this child
    | otherwise = FPNode (fpitem tree) (fpcount tree) (FPNode key 0 [] : fpchildren tree) -- Add key to tree's children