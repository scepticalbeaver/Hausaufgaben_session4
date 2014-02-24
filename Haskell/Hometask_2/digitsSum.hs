import Test.QuickCheck

digitsSum :: Int -> Int
digitsSum 0 = 0
digitsSum n = (n `mod` 10) + digitsSum (n `div` 10)

test_digitsSumSimple = digitsSum 99999 == 45

main = quickCheck test_digitsSumSimple