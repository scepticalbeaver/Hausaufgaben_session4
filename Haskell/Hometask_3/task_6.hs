--Regular Bracket sequence

checkSeq :: [Char] -> Bool
checkSeq list = calcBrackets list 0 where
    calcBrackets [] stack = 0 == stack 
    calcBrackets (x : xs) stack 
        | stack < 0 = False
        | otherwise = calcBrackets xs (update stack x)
    update :: (Num a) => a -> Char -> a
    update stack x
        | x == '(' = stack + 1
        | x == ')' = stack - 1
        | otherwise = stack


test = checkSeq "(" == False
        && checkSeq "" == True
        && checkSeq "()()(())" == True
        && checkSeq ")(" == False