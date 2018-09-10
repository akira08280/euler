{--
  https://projecteuler.net/problem=48
--}

module Euler48 (e48_solve) where

e48_solve :: Integral a => a
e48_solve = mod (sum . zipWith (^) [1..10 ^ 3] $ [1..10 ^ 3]) (10 ^ 10)
