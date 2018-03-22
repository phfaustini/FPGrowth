module Main where

import Control.Parallel
import MyTree

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = n1 `par` n2 `pseq` (n2 + n1)
    where
    n1 = fib (n - 1)
    n2 = fib (n - 2)


main::IO ()
main = print (fib 36)