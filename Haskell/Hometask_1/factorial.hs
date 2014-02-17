foldl2 fxy acc [] = acc
foldl2 fxy acc xs = foldl2 (fxy) (fxy acc (head xs)) (tail xs)

factor1 n = foldl2 (\ x y -> x * y) 0 [1..n]

factor2 n = product [1..n]
