import Test.QuickCheck

reverse2 :: [a] -> [a]
reverse2 [] = []
reverse2 xs =
    let
        revAux :: [a] -> [a] -> [a]
        revAux [] buff = buff
        revAux (x:xs) buff = revAux xs (x : buff)
    in revAux xs []


test_revers :: [Int] -> Bool
test_revers xs = reverse2 xs == reverse xs


main = quickCheck test_revers