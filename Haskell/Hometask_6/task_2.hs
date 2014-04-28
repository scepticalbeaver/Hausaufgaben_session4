data Monom a = Monom 
            { 
                coeff :: a
                , deg :: Int
            }
    deriving Eq

instance (Show a, Num a, Eq a) => Show (Monom a)
    where
    show(Monom c d) = 
        case (c, d) of
        (0, _) -> "0"
        (c, 0) -> show c
        (1, 1) -> "x"
        (1, d) -> "x^" ++ show d
        (c, 1) -> show c ++ "x"
        (c, d) -> show c ++ "x^" ++ show d


newtype Polynom a = Polynom 
                    { 
                        monoms :: [Monom a] 
                    }
                    deriving Eq

instance (Show a, Num a, Eq a) => Show (Polynom a)
    where
        show (Polynom []) = "0"
        show (Polynom xs) = show (head xs) ++ foldl (\acc x-> acc ++ " + " ++ show x) [] (tail xs)


(+::) :: Num a => Monom a -> Monom a -> [Monom a]
m1 +:: m2 =  if (deg m1 == deg m2) 
            then [Monom (coeff m1 + coeff m2) (deg m1)]
            else if (deg m1 > deg m2) then [m1, m2] else [m2, m1]

(*::) :: Num a => Monom a -> Monom a -> Monom a
m1 *:: m2 = Monom (coeff m1 * coeff m2) (deg m1 + deg m2)

addM :: Num a => Polynom a -> Monom a -> Polynom a
addM p m = Polynom $ addMonom (monoms p) m
    where 
    addMonom [] m = [m]
    addMonom arr@(x : xs) m =   if (deg x > deg m) then x : addMonom xs m
                                else    if (deg x == deg m) then(x +:: m) ++ xs else m : x : xs


multiply :: Num a => Polynom a -> Monom a -> Polynom a
multiply p m = Polynom $ map (\x -> x *:: m) (monoms p)

(+:) :: Num a => Polynom a -> Polynom a -> Polynom a
p1 +: p2 = foldr (\x acc -> addM acc x) p1 (monoms p2)

(*:) :: Num a => Polynom a -> Polynom a -> Polynom a
p1 *: p2 = summUp $ map (\m -> multiply p2 m) (monoms p1)
    where 
    summUp xs = foldr (\p acc -> p +: acc) (Polynom []) xs

--x + 1
simplePoly =  Polynom [Monom 1 1, Monom 1 0]

polyOne = Polynom [Monom 7 7, Monom 3 3, Monom 21 0]
polyTwo = Polynom [Monom 5 5, Monom 2 3, Monom 1 1]


summTest = (polyOne +: polyTwo) == Polynom [Monom 7 7, Monom 5 5, Monom 5 3, Monom 1 1, Monom 21 0]

multiplyTest = (simplePoly *: simplePoly) == Polynom [Monom 1 2, Monom 2 1, Monom 1 0]