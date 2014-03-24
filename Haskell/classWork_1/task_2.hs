data Tree a = Leaf a
            | Branch (Tree a) a (Tree a)
            deriving (Show)

-- fold from hometasks
foldrTree :: (a -> b -> b) -> b -> Tree a -> b
foldrTree fxy acc (Leaf x) = fxy x acc
foldrTree fxy acc (Branch l x r) = foldrTree fxy middlePart r
    where
    leftPart = foldrTree fxy acc l
    middlePart = fxy x leftPart

positiveOnly :: Tree Int -> [Int]
positiveOnly = foldrTree (\x acc -> if (x < 0) then x : acc else acc) []

tree1 = Branch 
        (Branch
            (Leaf (-2))
            5
            (Branch
                (Leaf 6)
                (-7)
                (Leaf 9)
            )

        ) 
        10
        (Leaf 13)

test = positiveOnly tree1 == [-7, -2]