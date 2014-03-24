--Дано выражение, содержащее переменную, константы, арифметические операции.
-- Посчитать производную этого выражения по этой переменной, провести в полученном
-- выражении для производной возможные упрощения (например, 1*x = x).                   Подходящий тип данных разработать самостоятельно
module Task6 where

data Expression = Term | Plus Expression Term
data Term = Factor | Product Term Factor
data Factor = Const Int | Variable | Power Expression Int | Expression

instance Show Factor
    where
    show (Const x) = show x
    show Variable = "x"
    show (Power a d) = show a ++ " ^ " ++ show d

instance Show Term
    where
    show (Product a b) = show a ++ " * " ++ show b

instance Show Expression
    where
    show (Plus a b) = show a ++ " + " ++ show b



simpleExpr = Plus (Const 5) Variable