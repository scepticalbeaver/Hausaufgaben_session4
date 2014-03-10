--Regular Bracket sequence

import Data.List

checkSeq :: [Char] -> Bool
checkSeq list = calcBrackets (list, [])
    where
    calcBrackets :: ([Char], [Char]) -> Bool
    calcBrackets ([], stack) = stack == []
    calcBrackets ((x : xs), stack) =
        if isOpenBr x then calcBrackets (xs, x : stack)
        else if isClosingBr x then
            if canClose stack x then calcBrackets (xs, tail stack) else False
            else calcBrackets (xs, stack)
    
    canClose :: [Char] -> Char -> Bool
    canClose stack cymb =
        case stack of
            (s: ss) -> elemIndex s opens == elemIndex cymb closures
            _ -> False
        

    opens = ['{', '(', '[', '<']
    closures = ['}', ')', ']', '>']

    isOpenBr :: Char -> Bool
    isOpenBr cymb = elem cymb opens
    isClosingBr :: Char -> Bool
    isClosingBr cymb = elem cymb closures


test = checkSeq "(" == False
        && checkSeq "" == True
        && checkSeq "()()(())" == True
        && checkSeq ")(" == False
        && checkSeq "<<()()[()]>>([][])" == True
        && checkSeq "<<>[>" == False
        && checkSeq ">_<" == False