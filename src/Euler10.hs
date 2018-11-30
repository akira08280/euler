{--
  https://projecteuler.net/problem=10
--}

module Euler10 (e10Solve) where

import Data.Numbers.Primes (primes)

e10Solve :: Integral a => a
e10Solve = sum . takeWhile (< 2 * 10 ^ 6) $ primes
