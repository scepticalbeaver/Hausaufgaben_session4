module Task6 where

--data Expression = Term | Plus Expression Term
--data Term = Factor | Product Term Factor
--data Factor = Const Int | Variable | Power Expression Int | Expression

data Expression = Plus Expression Expression
                | Product Expression Expression
                | Power Expression Int
                | Variable
                | Const Int

infixl 6 `Plus`
infixl 7 `Product`
infixl 8 `Power`

instance Show Expression
    where
    show (Plus a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
    show (Product a b) = "(" ++ show a ++ " * " ++ show b ++ ")"
    show (Power a d) = "(" ++ show a ++ "^(" ++ show d ++ "))"
    show Variable = "x"
    show (Const x) = show x

derivate :: Expression -> Expression
derivate (Const x) = Const 0
derivate Variable = Const 1
derivate (Plus a b) = (derivate a) `Plus` (derivate b)
derivate (Product a b) = ((derivate a) `Product` b) `Plus` ((derivate b) `Product` a) 
derivate (Power x z) = ((Const z) `Product` (x `Power` (z - 1))) `Product` (derivate x)

simplify' :: Expression -> Bool -> Expression
simplify' Variable _ = Variable
simplify' (Const x) _ = Const x
simplify' (Product x y) guard = case (simplify' x True, simplify' y True) of
                                (Const a, Const b) -> Const (a * b)
                                (Const 0, _) -> Const (0)
                                (_, Const 0) -> Const (0)
                                (Const 1, z) -> simplify' z True
                                (z, Const 1) -> simplify' z True
                                (Variable, Power Variable d) -> Variable `Power` (d + 1)
                                (Power Variable d, Variable) -> Variable `Power` (d + 1)
                                (z, Const k) -> simplify' ((Const k) `Product` (simplify' z True)) True
                                (Const k, z) -> Const k `Product` (simplify' z True)
                                --(Const k1, ((Const k2) `Product` z)) -> (Const k1 * k2) `Product` (simplify' z True)
                                (l, r) -> if guard 
                                          then Product (simplify' l False) (simplify' r False)
                                          else Product l r
simplify' (Plus x y) guard = case (simplify' x True, simplify' y True) of
                            (Const a, Const b) -> Const (a + b)
                            (Const 0, z) -> simplify' z True
                            (z, Const 0) -> simplify' z True
                            (Variable, Variable) -> Const 2 `Product` Variable
                            (Const a, Variable) -> Variable `Plus` Const a
                            (l, r) -> if guard
                                      then Plus (simplify' l False) (simplify' r False)
                                      else Plus l r
simplify' (Power Variable 0) _ = Const 1
simplify' z _ = z

--siftDown :: Expression -> Expression -> Maybe Expression
--siftDown (Count k) sub = case sub of
--                         (Const k2, z) -> Just ((Const (k * k2)), z) 
--                         otherwise -> Nothing

simplify :: Expression -> Expression
simplify z = simplify' z True

simpleExpr = (Const 5) `Plus` Variable `Product` Variable  -- "5 + x * x"


polynomOfFour = (Power Variable 3) `Plus` Variable `Product` (Power Variable (-1)) `Plus` (Const 2) `Product` (Power Variable 4)
polyNON = (Const 2) `Product` (Const 3) `Plus` (Const 4)


make = simplify . derivate . simplify

test1 = make simpleExpr  --(2 * x)