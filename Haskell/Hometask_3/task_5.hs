data Tree a = Leaf a
            | Branch (Tree a) a (Tree a)
            deriving (Show)



t1 = Branch 
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

height :: (Ord a, Num a) => Tree a -> a
height (Leaf _) = 1
height (Branch l _  r) = 1 + max (height l) (height r)

minLenToLeaf :: (Ord a, Num a) => Tree a -> a
minLenToLeaf (Leaf _) = 0
minLenToLeaf (Branch l _  r) = 1 + min (minLenToLeaf l) (minLenToLeaf r)


test = height t1 == 4 
        && minLenToLeaf t1 == 1
