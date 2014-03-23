module Task1 where

import Data.List
import Control.Monad

doDecompose :: Int -> Int -> [Int] -> IO()
doDecompose 0 _ xs = putStrLn (show xs)
doDecompose n mink xs = 
    do 
    forM_ [mink..n] (\i -> if (i >= mink) 
                            then doDecompose (n-i) i (xs ++ [i]) 
                            else return ())

main = do
    putStr "Put some integer:\n> "
    n <- getLine
    doDecompose (read n :: Int) 1 []