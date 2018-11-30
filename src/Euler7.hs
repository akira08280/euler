{--
  https://projecteuler.net/problem=7
--}

module Euler7 (e7Solve) where

import Data.Numbers.Primes (primes)

e7Solve :: Integral a => a
e7Solve = primes !! 10000
