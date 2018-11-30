{--
  https://projecteuler.net/problem=56
--}

module Euler56 (e56Solve) where

import Data.Char (digitToInt)

e56Solve :: Int
e56Solve = maximum . map (sum . map digitToInt . show) $ (^) <$> [1..10 ^ 2] <*> [1..10 ^ 2]
