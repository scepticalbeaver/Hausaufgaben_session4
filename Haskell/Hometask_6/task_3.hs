data BSTree a =   Node a (BSTree a) (BSTree a)
                | Empty
    deriving (Show, Eq)

insert :: Ord a => a -> BSTree a -> BSTree a
insert x Empty = Node x Empty Empty
insert x (Node curr left right)
    | x == curr = Node curr left right
    | x < curr = Node curr (insert x left) right
    | x > curr = Node curr left (insert x right)


remove :: Ord a => a -> BSTree a -> BSTree a
remove x Empty = Empty
remove x (Node curr left right)
        | x == curr = delete $ Node curr left right
        | x < curr  = Node curr (remove x left) right
        | x > curr  = Node curr left (remove x right)
    where 
    delete :: (Ord a) => BSTree a -> BSTree a
    delete (Node _ Empty Empty) = Empty
    delete (Node _ Empty right) = right
    delete (Node _ left Empty) = left
    delete (Node curr left right) = 
        if left /= Empty then Node leftMostRight (remove leftMostRight left) right
                         else Node rightMostLeft left (remove rightMostLeft right)
        where
        leftMostRight = popMostRight left
        rightMostLeft = popMostLeft right

        popMostRight :: (Eq a) => BSTree a -> a
        popMostRight (Node curr left right) =
            if right == Empty then curr
            else popMostRight right

        popMostLeft :: (Eq a) => BSTree a -> a
        popMostLeft (Node curr left right) =
            if left == Empty then curr
            else popMostLeft left

find :: Ord a => a -> BSTree a -> Bool
find x Empty = False
find x (Node curr left right)
    | x == curr = True
    | x < curr = find x left
    | x > curr = find x right

height :: (Num a, Ord a) => BSTree t -> a
height Empty = 0
height (Node _ left right) = 1 + max  (height left) (height right)

foldrTree :: (a -> b -> b) -> b -> BSTree a -> b
foldrTree fxy acc Empty = acc
foldrTree fxy acc (Node x left right) = foldrTree fxy middlePart right
    where
    leftPart = foldrTree fxy acc left
    middlePart = fxy x leftPart

size tree = foldrTree (\x acc -> 1 + acc) 0 tree


oneLevelTree = Node
                5
                Empty
                Empty

grossBaume =    Node
                50
                (
                    Node
                    30
                    (
                        Node 
                        10
                        (
                            Node
                            0
                            Empty Empty
                        )
                        (
                            Node 
                            20
                            Empty Empty
                        )
                    )
                    (
                        Node
                        40
                        Empty Empty
                    )
                )
                (
                    Node
                    90
                    Empty
                    (
                        Node
                        100
                        Empty Empty
                    )
                )


heightTest = height grossBaume == 4 && height oneLevelTree == 1
findTest = find 20 grossBaume && not (find 49 grossBaume)
insertTest = find 47 (insert 47 grossBaume)
removeTest = find 30 grossBaume && not(find 30 (remove 30 grossBaume)) && find 10 (remove 30 grossBaume)
sizeTest = size oneLevelTree == 1 && size grossBaume == 8

totalTest = heightTest
            && findTest
            && insertTest
            && removeTest
            && sizeTest