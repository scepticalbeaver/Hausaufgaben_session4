import Test.QuickCheck

fold fxy acc [] = acc
fold fxy acc xs = fold (fxy) (fxy acc (head xs)) (tail xs)

factor :: Integer -> Integer
factor n = fold (\ x y -> x * y) 1 [1..n]

test_factor n = factor n == product [1..n]


main = quickCheck test_factor