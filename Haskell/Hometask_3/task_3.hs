-- That's not ready yet...

sample = [1,7,9,11,17,19,71,77,79,91]

a = take 10 list
a' = take 10 listCorrect

b = (a == a')

-- a[i] = 10 * a[i div 3] + a[i % 3]

listCorrect = map getX [1..] 
    getX x 
        | x == 1 = 1
        | x == 2 = 7
        | x == 3 || x == 0 = 9
        | x > 0 = 10 * (getX ((x - 1) `div` 3)) + getX (x `mod` 3) -- it isn't real exp algo, its fast O(N) 
        | otherwise = error "Positive only"


--list = 1 : 7 : 9 : zipWith(+) (zipWith(*) ([10, 10 ..]) (list)) (list) 

modes = 1 : 7 : 9 : modes
dives = 0 : 0 : 0 : zipWith(*) [10, 10..] list
list = zipWith(+) dives modes