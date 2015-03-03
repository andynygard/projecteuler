module Common where

-- |A list containing every prime number
primes = 2 : filter ((==1) . length . primeFactors) [3,5..]

-- |A list containing the prime factors of the given number
primeFactors a = factor a primes
    where
        factor a (x:xs) 
            | a < x ^ 2         = [a]
            | a `mod` x == 0    = x : factor (a `div` x) (x:xs)
            | otherwise         = factor a xs
