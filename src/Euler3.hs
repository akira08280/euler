{--
  https://projecteuler.net/problem=3
--}

module Euler3 (e3_solve) where

import NumberTheory (primes)

e3_solve :: Integral a => a
e3_solve = maximum . primes $ 600851475143
