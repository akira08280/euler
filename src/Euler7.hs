{--
  https://projecteuler.net/problem=7
--}

module Euler7 (e7_solve) where

import Data.Numbers.Primes (primes)

e7_solve :: Integral a => a
e7_solve = primes !! 10000
