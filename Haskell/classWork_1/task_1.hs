-- Eratosphen sift
primes :: [Integer]
primes = 2 : sieve [3, 5..] 
    where
    sieve (x : xs) = x : sieve [t | t <- xs, t `mod` x /= 0]

test = take 5 primes == [2, 3, 5, 7, 11]