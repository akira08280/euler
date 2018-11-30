{--
  https://projecteuler.net/problem=28
--}

module Euler28 (e28Solve) where

e28Solve :: Integral a => a
e28Solve = succ . sum . map (\n -> 4 * (2 * n + 1) ^ 2 - 12 * n) $ [1..500]
