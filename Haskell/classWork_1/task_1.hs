-- Eratosphen sift
import Data.List

primes = 2 : sieve [3, 5..] where
   sieve (x : xs) = x : sieve (xs \\ map (\a -> a * x) (x : xs))
