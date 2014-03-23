module Task2 where

satisfies :: (a -> Bool) -> [a] -> Bool
satisfies p = foldr (\x acc -> (p x) && acc) True

p x = x > 5

test = satisfies p [1..10] == False
        && satisfies p [10..20] == True