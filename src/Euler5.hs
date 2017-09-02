{--
  https://projecteuler.net/problem=5
--}

module Euler5 (e5_solve) where

e5_solve :: Integral a => a
e5_solve = foldl1 lcm [2..20]
