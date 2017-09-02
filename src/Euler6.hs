{--
  https://projecteuler.net/problem=6
--}

module Euler6 (e6_solve) where

e6_solve :: Integral a => a
e6_solve = round result
  where
    n = 100
    sum = (\n -> n * (n + 1) / 2)
    square = (\n -> (n * (n + 1) * (2 * n + 1)) / 6)
    result = (sum n) ^ 2 - square n
