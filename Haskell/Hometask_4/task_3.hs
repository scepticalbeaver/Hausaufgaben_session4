module Task_3 where

evenCounterFold :: (Integral a, Num b) => [a] -> b
evenCounterFold list = foldr (\x acc -> if isEven x then acc + 1 else acc) 0 list

evenCounterFilter :: (Integral a) => [a] -> Int
evenCounterFilter  = length . filter (\x -> isEven x)  

evenCounterMap :: (Integral a, Num b) => [a] -> b
evenCounterMap list = sum $ map (\x -> if isEven x then 1 else 0) list

isEven x = x `mod` 2 == 0

test = 
    evenCounterFold [1..10] == 5
    && evenCounterFold [1, 3 ,4 ,5, 5, 11, 21, 0] == 2
    && evenCounterFilter [] == 0
    && evenCounterFilter [1..10] == evenCounterFold [1..10]
    && evenCounterMap [1, 2] == 1
    && evenCounterMap [1..10] == evenCounterFold [1..10]


main = do
    putStr "All tests passed:\n"
    putStr (show(test) ++ "\n\n")