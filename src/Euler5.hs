{--
  https://projecteuler.net/problem=5
--}

module Euler5 (e5Solve) where

e5Solve :: Integral a => a
e5Solve = foldl1 lcm [2..20]
