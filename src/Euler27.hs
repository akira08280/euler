{--
  https://projecteuler.net/problem=27

  n ^ 2 + an + b
  (|a| < 1000, |b| <= 1000, n = [0..])

  Find the product of the coefficients, a and b,
  for the quadratic expression that produces the maximum number of primes for consecutive values of n,
  starting with n = 0.

  1. if n == 0, n ^ 2 + an + b = b
     Therefore b is prime.

  2. if n == 1, n ^ 2 + an + b = 1 + a + b
     1 + a + b is prime, so 1 + a + b > 1.
     Therefore a > -b.

  3. if a is even, then 1 + a + b is even.(b is odd prime)
     Therefore a is odd.
--}

module Euler27 (e27_solve) where

import Control.Monad (guard)
import Data.Ord (comparing)
import Data.List (maximumBy)
import Data.Numbers.Primes (isPrime, primes)

e27_solve :: Integral a => a
e27_solve = snd . maximumBy (comparing fst) . map (countPrimesFrom 2) $ coefficients

coefficients :: Integral a => [(a, a)]
coefficients = do
  a <- [-999,-997..999]
  b <- tail . takeWhile (<= 10 ^ 3) $ primes
  guard (a > negate b)
  return (a, b)

countPrimesFrom :: Integral a => a -> (a, a) -> (a, a)
countPrimesFrom n (a, b)
  | check = countPrimesFrom (succ n) (a, b)
  | otherwise = (n, (a * b))
  where
    check = isPrime (n ^ 2 + a * n + b)
