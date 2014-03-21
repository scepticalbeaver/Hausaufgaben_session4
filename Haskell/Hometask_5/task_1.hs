--Реализовать функцию, которая по заданному числу n выводит все его разложения на положительные слагаемые (с точностью до порядка разложения)
-- Task: decompose integer to sum
module Task1 where

import Data.List
import Control.Monad
--import Data.Set (toList, fromList)

--rmDublicates = toList . fromList

-- some exp algo
--decomp n = sort (helper n n)
--    where 
--    helper n 0 = [[]]
--    helper n len = rmDublicates (map (\ xs -> sort xs) (filter  (\ xs -> (sum xs) == n) (replicateM len [1..n]))) ++ (helper n (len - 1))

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