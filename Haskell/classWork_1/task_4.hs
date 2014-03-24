strangeFoo :: [Floating a] => [a] -> a
strangeFoo xs = (fst folded) / (snd folded)
    where folded = foldr (\x acc -> (x + (fst acc), (cos x) * (snd acc))) (0, 1) xs