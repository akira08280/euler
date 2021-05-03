{--
  https://projecteuler.net/problem=174
--}

module Euler174 (e174Solve) where

import Data.List (sort, group)

e174Solve :: Int
e174Solve = length . filter (< 11) . map length . group . sort $ f

f :: [Integer]
f = do
  k <- [1..500]
  let
    start = succ k
    upper = 250000 `div` k
  j <- [start..upper]
  return $ k * j
