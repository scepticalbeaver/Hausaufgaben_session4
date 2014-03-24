import Data.List

indexOf :: (Eq a) => a -> [a] -> Maybe Int
indexOf x xs = findIndex (\a -> a == x) xs

test = (indexOf 100 [1..]) == Just 99