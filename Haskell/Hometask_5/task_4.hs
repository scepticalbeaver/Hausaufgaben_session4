module Task_4 where

import Data.List

isListUniq :: (Ord a) => [a] -> Bool
isListUniq [] = True
isListUniq list = not $ hasSameNeighb $ sort list
    where
    hasSameNeighb [x] = False
    hasSameNeighb (x : xs) = x == head xs || hasSameNeighb xs

-- in case of non-ordered list
isListUniq2 :: (Eq a) => [a] -> Bool
isListUniq2 [] = True
isListUniq2 (x : xs) = if x `elem` xs then False else isListUniq2 xs

test = 
    isListUniq [1..10] == True
    && isListUniq (7 : [1..10]) == False

main = do
    putStr "All tests passed:\n"
    putStr (show(test) ++ "\n\n")