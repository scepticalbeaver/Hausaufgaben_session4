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
    show (Product a b) = show a ++ " * " ++ show b
    show (Power a d) = show a ++ "^(" ++ show d ++ ")"
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
                                (Variable, Variable) -> Power Variable 2
                                (Const 0, _) -> Const (0)
                                (_, Const 0) -> Const (0)
                                (Const 1, z) -> simplify' z True
                                (z, Const 1) -> simplify' z True
                                (Variable, Power Variable d) -> Variable `Power` (d + 1)
                                (Power Variable d, Variable) -> Variable `Power` (d + 1)
                                (z, Const k) -> simplify' ((Const k) `Product` (simplify' z True)) True
                                (Const k1, z) -> siftDownProduction (Const k1) (simplify' z True)
                                (Variable, z) -> siftDownProduction (Variable) (simplify' z True)
                                (l, r) -> if guard 
                                          then Product (simplify' l False) (simplify' r False)
                                          else Product l r

simplify' (Plus x y) guard = case (simplify' x True, simplify' y True) of
                            (Const a, Const b) -> Const (a + b)
                            (Const 0, z) -> simplify' z True
                            (z, Const 0) -> simplify' z True
                            (Variable, Variable) -> Const 2 `Product` Variable
                            (Const a, Variable) -> Variable `Plus` Const a
                            (Const k, z) -> siftDownAddition (Const k) (simplify' z True)
                            (z, Const k) -> siftDownAddition (Const k) (simplify' z True) 
                            (l, r) -> if guard
                                      then Plus (simplify' l False) (simplify' r False)
                                      else Plus l r
simplify' (Power Variable 0) _ = Const 1
simplify' z _ = z

siftDownProduction :: Expression -> Expression -> Expression
siftDownProduction coeff sub = case sub of
                        (Variable) -> Product coeff Variable
                        (Product x y) ->  Product (simplify $ siftDownProduction coeff x)  y
                        (Plus x y) -> Plus (simplify $ siftDownProduction coeff x) (simplify $ siftDownProduction coeff y)
                        other -> Product coeff other

siftDownAddition :: Expression -> Expression -> Expression
siftDownAddition coeff sub = case (coeff, sub) of
                        (coeff, Variable) -> Plus coeff Variable
                        (Const k, Plus x y) -> Plus x (simplify $ siftDownAddition (Const k) y)
                        (z, Plus x y) -> Plus (simplify $ siftDownAddition coeff x) y
                        (Const k, z) -> Plus z coeff
                        (coeff, other) -> Plus coeff other

simplify :: Expression -> Expression
simplify z = simplify' z True


make = simplify . derivate . simplify

------------ Test cases

-- Polynom examples

-- "5 + x * x"
simpleExpr = (Const 5) `Plus` Variable `Product` Variable

-- x^3 + x * x^(-1) + 2 * x^4
polynomOfFour = (Power Variable 3) `Plus` Variable `Product` (Power Variable (-1)) `Plus` (Const 2) `Product` (Power Variable 4)

-- 2 * 3 + 4
justExpr = (Const 2) `Product` (Const 3) `Plus` (Const 4)

-- x * (x + 3)
treeCheckProduct = (Variable) `Product` (Variable `Plus` Const 3)

-- 3 + (x + 4)
treeCheckPlus = (Const 3) `Plus` (Variable `Plus` Const 4)

test0 = make simpleExpr  --(2 * x)

test_s1 = simplify simpleExpr       -- x^2 + 5
test_s2 = simplify polynomOfFour    -- x^3 + 1 + 2 * x^4
test_s3 = simplify treeCheckProduct -- x^2 + 3*x
test_s4 = simplify treeCheckPlus    -- x + 7

test = make polynomOfFour --8 * x^3 + 3 * x^2