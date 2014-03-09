import Data.List

findMaxNeighbours :: (Enum a, Num a, Ord a) => [a] -> a
findMaxNeighbours [] = 0
findMaxNeighbours [x] = 1
findMaxNeighbours (x : xs) =
    snd (maximumBy maxFst (zip (zipWith(+) (0 : x : xs) (xs ++ [0] ++ [0])) [1..])) where

        maxFst :: (Ord a) => (a, b) -> (a, b) -> Ordering
        maxFst a b 
            | fst a < fst b = LT
            | otherwise = GT


test =
    findMaxNeighbours [1, 5, 6, 2] == 2
    && findMaxNeighbours [1,7,9,11,17,19,71,77,79,91] == 9
    && findMaxNeighbours [0, 9, 0] == 1
    && findMaxNeighbours [1, 2, 3, 12, 0, 12, 0, 12, 3, 2, 1] == 5
