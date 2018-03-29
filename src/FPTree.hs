module FPTree where
    
data MyNode = MyNode String Integer deriving (Show)

data FPTree = FPTree MyNode [FPTree] deriving (Show)