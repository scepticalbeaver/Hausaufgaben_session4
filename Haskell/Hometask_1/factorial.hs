foldl2 fxy acc [] = acc
foldl2 fxy acc xs = foldl2 (fxy) (fxy acc (head xs)) (tail xs)


foldl2 (\ x y -> x + y) 0 [1..10]