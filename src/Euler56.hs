{--
  https://projecteuler.net/problem=56
--}

module Euler56 (e56_solve) where

import Data.Char (digitToInt)

e56_solve :: Int
e56_solve = maximum . map (sum . map digitToInt . show) $ (^) <$> [1..10 ^ 2] <*> [1..10 ^ 2]
