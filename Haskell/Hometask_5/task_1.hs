module Task_1 where

data Tree a = Leaf a
            | Branch (Tree a) a (Tree a)
            deriving (Show)

filterTree :: (a -> Bool) -> Tree a -> [a]
filterTree p (Leaf x) = if p x then [x] else []
filterTree p (Branch l x r) = if p x then l1 ++ x : l2 else l1 ++ l2
    where
    l1 = filterTree p l
    l2 = filterTree p r

tree1 = Branch 
        (Branch
            (Leaf 2)
            5
            (Branch
                (Leaf 6)
                7
                (Leaf 9)
            )

        ) 
        10
        (Leaf 13)

leafTree = Leaf 21

test = filterTree (\x -> x > 9) tree1 == [10, 13]
        && filterTree (\x -> x < 7) tree1 == [2, 5, 6]
        && filterTree (\x -> x^2 > 300) leafTree == [21]
        && filterTree (\x -> x < 19) leafTree == []


main = do
    putStr "All tests passed:\n"
    putStr (show(test) ++ "\n\n")