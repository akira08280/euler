{--
  https://projecteuler.net/problem=3
--}

module Euler3 (e3Solve) where

import NumberTheory (primes)

e3Solve :: Integral a => a
e3Solve = maximum . primes $ 600851475143
