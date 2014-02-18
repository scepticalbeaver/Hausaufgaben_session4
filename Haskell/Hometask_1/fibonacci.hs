import Test.QuickCheck

-- problem block: Fibonacci

-- aux function: fib list builder
makeFibList :: Integer -> [Integer] -> [Integer]
makeFibList 2 xs = xs
makeFibList n xs = makeFibList (n - 1) (head xs + (head (tail xs)) : xs)


fiboNum :: Integer -> Integer
fiboNum 1 = 1
fiboNum 2 = 1
fiboNum n
		| n < 1 = error "N must be positive"
		| otherwise = head (makeFibList n [1, 1])


-- Testing block

canonicalFib n
		| n == 1 = 1
		| n == 2 = 1
		| otherwise = canonicalFib (n - 1) + canonicalFib (n - 2)

test_fiboNum (Positive n) -- Unfortunately, canonical algo is too long for big numbers
		| n > 29 = True
		| otherwise = fiboNum n == canonicalFib n

main = quickCheck test_fiboNum