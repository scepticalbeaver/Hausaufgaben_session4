import Test.QuickCheck

isPolyndrome :: Eq a => [a] -> Bool
isPolyndrome [] = True
isPolyndrome xs = polyAux xs (reverse xs) where
    polyAux :: Eq a => [a] -> [a] -> Bool
    polyAux (x : []) (y : []) = x == y
    polyAux (x : xs) (y : ys) = if x /= y then False else polyAux xs ys

simpleTest = 
    isPolyndrome "12221" == True
    && isPolyndrome "12223" == False


main = quickCheck simpleTest