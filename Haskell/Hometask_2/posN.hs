import Test.QuickCheck

posN :: Integer -> [Integer] -> Integer
posN n [] = -1
posN n xs = posAux n xs 1 where
    posAux :: Integer -> [Integer] -> Integer -> Integer
    posAux _ [] _ = -1
    posAux n (x : xs) k 
        | x == n = k
        | otherwise = posAux n xs (k + 1)

otherPos :: Integer -> [Integer] -> Integer
otherPos n [] = -1
otherPos n xs = head (foldr (\ (f,s) acc -> if s == n then (f : acc) else acc) [] (zip [1, 2..] xs))

test_poses n = (posN n xs) == (otherPos n xs) where
    xs = ([1..500] ++ (n : [501..1000]))

main = quickCheck test_poses