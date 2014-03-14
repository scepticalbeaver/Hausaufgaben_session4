module Task_2 where

import Task_1 hiding (test, main)


foldrTree :: (a -> b -> b) -> b -> Tree a -> b
foldrTree fxy acc (Leaf x) = fxy x acc
foldrTree fxy acc (Branch l x r) = foldrTree fxy middlePart r
    where
    leftPart = foldrTree fxy acc l
    middlePart = fxy x leftPart

test = 
     foldrTree (\x acc -> x + acc) 0 tree1 == 52
     && foldrTree (\x acc -> x + acc) 0 leafTree == 21
     && foldrTree (\x acc -> x : acc) [] tree1 == [13, 10, 9, 7, 6, 5, 2]


main = do
    putStr "All tests passed:\n"
    putStr (show(test) ++ "\n\n")