{--
  https://projecteuler.net/problem=174
--}

module Euler174 (e174Solve) where

import Data.List (sort, group)

e174Solve :: Int
e174Solve = length . filter (< 11) . map length . group . sort $ do
  m <- [1..499]
  let
    start = succ m
    upper = 250000 `div` m
  n_m <- [start..upper]
  return $ m * n_m
