{--
  https://projecteuler.net/problem=48
--}

module Euler48 (e48Solve) where

e48Solve :: Integral a => a
e48Solve = mod (sum . zipWith (^) [1..10 ^ 3] $ [1..10 ^ 3]) (10 ^ 10)
