--pattern a[i] = 10 * a[i div 3] + a[i % 3]
listCorrect = map getX [1..] where
    getX x 
        | x == 1 = 1
        | x == 2 = 7
        | x == 3 || x == 0 = 9
        | x > 0 = 10 * (getX ((x - 1) `div` 3)) + getX (x `mod` 3) -- it isn't real exp algo, its fast O(N) 
        | otherwise = error "Positive only"



-- Answer
mods = 1 : 7 : 9 : mods
list = 1 : 7 : 9 : zipWith(+) (foldr (\x acc -> (10 * x) : (10 * x) : (10 * x) : acc) [] list) mods


test = take 20 list == take 20 listCorrect