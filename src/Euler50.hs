{--
  https://projecteuler.net/problem=50

  Let f is the accumulation of primes.

  index |  0 | 1 | 2 |  3 |  4 |  5 |  6 |  7 |  8 |
  --------------------------------------------------
  p     |    | 2 | 3 |  5 |  7 | 11 | 13 | 17 | 19 |
  --------------------------------------------------
  f(x)  |  0 | 2 | 5 | 10 | 17 | 28 | 41 | 58 | 77 |

  1) In case of 28. f(5)
     28 - f(0) = 28 (not prime) index is 5.
     28 - f(1) = 26 (not prime) index is 4.
     28 - f(2) = 23 (prime)     index is 3.
     Therefore, 23 is the sum of three consecutive primes.
     (5 + 7 + 11 = 23)

  2) In case of 41. f(6)
     41 - f(0) = 41 (prime)     index is 6.
     Therefore, 41 is the sum of six consecutive primes.
     (2 + 3 + 5 + 7 + 11 + 13 = 41)

  3) In case of 77. f(8)
     77 - f(0) = 77 (not prime) index is 8.
     77 - f(1) = 75 (not prime) index is 7.
     77 - f(2) = 72 (not prime) index is 6.
     77 - f(3) = 67 (prime)     index is 5.
     Therefore, 67 is the sum of five consecutive primes.
     (7 + 11 + 13 + 17 + 19 = 67)
--}

module Euler50 (e50Solve) where

import Data.Ord (comparing)
import Data.List (maximumBy)
import Data.Numbers.Primes (isPrime, primes)

e50Solve :: Int
e50Solve = snd . maximumBy (comparing fst) . zipWith (sumOfConsecutivePrimes 0) [1..] $ tail f

f :: Integral a => [a]
f = takeWhile (< 10 ^ 6) . scanl (+) 0 $ primes

sumOfConsecutivePrimes :: Int -> Int -> Int -> (Int, Int)
sumOfConsecutivePrimes x index acc
  | isPrime acc' = (index, acc')
  | otherwise    = sumOfConsecutivePrimes (succ x) (pred index) acc
  where
    acc' = (`subtract` acc) . (f !!) $ x
