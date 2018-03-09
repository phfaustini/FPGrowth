module MyTree where
    
data MyNode = MyNode String Integer deriving (Show)

data MyTree = MyTree MyNode [MyTree] deriving (Show)