{--
  https://projecteuler.net/problem=97
--}

module Euler97 (e97Solve) where

e97Solve :: Integer
e97Solve = ((28433 * 2 ^ 7830457) + 1) `mod` (10 ^ 10)
