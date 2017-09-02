{--
  https://projecteuler.net/problem=10
--}

module Euler10 (e10_solve) where

import Data.Numbers.Primes (primes)

e10_solve :: Integral a => a
e10_solve = sum . takeWhile (< 2 * 10 ^ 6) $ primes
