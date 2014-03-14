module SortedList where

add :: (Ord a) =>  a -> [a] -> [a]
add a [] = [a]
add a list@(x : xs) = if a < head list then a : list else x : add a xs

remove :: (Ord a) => a -> [a] -> [a]
remove a [] = []
remove a list@(x : xs) = 
    if a < x then list else
        if a == x then remove a xs else x : remove a xs
