{--
  https://projecteuler.net/problem=97
--}

module Euler97 (e97_solve) where

e97_solve :: Integer
e97_solve = ((28433 * 2 ^ 7830457) + 1) `mod` (10 ^ 10)
