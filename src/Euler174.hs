{--
  https://projecteuler.net/problem=174

  Let n be outer tiles count and let m be the outer layer count.
  Therefore the formula to count all tiles is

  t = 1000000 = n^2 - (n - 2m)^2 = 4m(n - m)
  t/4 = 250000 = m(n - m) and 2m < n

  So,

  m(n - m) < 250000 and n - m > m

--}

module Euler174 (e174Solve) where

import Data.List (sort, group)

e174Solve :: Int
e174Solve = length . filter (< 11) . map length . group . sort $ do
  k <- [1..500]
  let
    start = succ k
    upper = 250000 `div` k
  j <- [start..upper]
  return $ k * j
