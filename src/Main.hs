module Main where

-- https://stackoverflow.com/questions/11229854/how-can-i-parse-the-io-string-in-haskell

import Control.Parallel
import MyTree

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = n1 `par` n2 `pseq` (n2 + n1)
    where
    n1 = fib (n - 1)
    n2 = fib (n - 2)




readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

parseTransactions lines output
    | null lines = output
    | otherwise = parseTransactions (tail lines) (words $ head lines) ++ output


main::IO ()
main = do
    lines <- readLines "input/transactions2.txt"
    print lines