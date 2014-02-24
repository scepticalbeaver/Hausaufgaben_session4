import Test.QuickCheck

powerOfTwo :: Int -> [Integer]
powerOfTwo n =
    map (\x -> 2^x) [0..n]


powerOfTwo2 :: Int -> [Integer]
powerOfTwo2 0 = [1]
powerOfTwo2 n = (pow2Aux 0 n)  where
    pow2Aux :: Int -> Int -> [Integer]
    pow2Aux a b | a == b = 2^a : []
    pow2Aux a b = 2^a : pow2Aux (a + 1) b



test_powersTwo (Positive n) 
    | n > 10000 = True  -- too long
    | otherwise = powerOfTwo n == powerOfTwo2 n

main = quickCheck test_powersTwo