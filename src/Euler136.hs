{--
  https://projecteuler.net/problem=136

  1) n = 4 * p     (p is odd prime, or p = 1)
  2) n = 4 * 4 * p (p is odd prime, or p = 1)
  3) n = p         (p is prime, and p mod 4 = 3)
--}

module Euler136 (e136_solve) where

import Math.NumberTheory.Primes.Sieve (primeList, primeSieve)

-- The reason for adding 2 is because it is necessary to consider cases 4 and 16.
e136_solve :: Int
e136_solve = (+ 2) . sum . map countN . tail . sieve $ limit

sieve :: Integer -> [Integer]
sieve = primeList . primeSieve

countN :: Integral t => t -> Int
countN p = length . filter id $ [cond1, cond2, cond3]
  where
    cond1 = p < limit `div` 4
    cond2 = p < limit `div` 16
    cond3 = p + 1 `mod` 4 == 0

limit :: Integral a => a
limit = 5 * 10 ^ 7
