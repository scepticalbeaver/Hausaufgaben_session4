module Task_1 where

data CharTree a =    Node Char (CharTree a)(CharTree a)
                    | Empty 
                    deriving Eq


instance (Show a) => Show (CharTree a)
    where
    show (Empty) = "e"
    show (Node x left right) = "n" ++ show x ++ show left ++ show right

{-instance (Eq tree) => Eq (CharTree tree)
    where
    (Empty) == (Empty) = True
    (Node x left1 right1) == (Node y left2 right2) = (x == y) && (left1 == left2) && (right1 == right2)
-}

serializeTree :: CharTree tree -> [Char]
serializeTree tree = serialize tree []
    where
    serialize :: CharTree a ->  [Char] -> [Char]
    serialize (Empty) acc = 'e' : acc
    serialize (Node x left right) acc = 'n' : x : (serialize left rightPart)
        where
        rightPart = serialize right acc


buildTree :: [Char] -> CharTree tree
buildTree xs = fst (buildTreeAux xs) 
    where
    buildTreeAux (x : xs) =
        case x of
        'n' -> (Node (head xs) subLeft subRight, rest) 
            where 
            (subRight, rest) = (buildTreeAux xxs)
            (subLeft, xxs) = (buildTreeAux $ tail xs)
            
        'e' -> (Empty, xs)

--test :: Bool
test =  tree == buildTree (serializeTree oneLevelTree)

emptyTree = Empty

oneLevelTree = Node 
                'A'
                Empty
                Empty


tree =  
        Node 
            'F'
            (
                Node
                'R'
                (
                    Node
                    'U'
                    Empty
                    Empty
                )
                (
                    Node
                    'I'
                    Empty
                    Empty
                )
            )
            (
                Node
                'T'
                (
                    Node
                    'S'
                    Empty
                    Empty
                )
                Empty
            )