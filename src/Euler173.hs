{--
  https://projecteuler.net/problem=173
--}

module Euler173 (e173Solve) where

e173Solve :: Integer
e173Solve = sum . map (\m -> 250000 `div` m - m) $ [1..499]
