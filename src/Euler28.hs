{--
  https://projecteuler.net/problem=28
--}

module Euler28 (e28_solve) where

e28_solve :: Integral a => a
e28_solve = succ . sum . map (\n -> 4 * (2 * n + 1) ^ 2 - 12 * n) $ [1..500]
