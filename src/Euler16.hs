{--
  https://projecteuler.net/problem=16
--}

module Euler16 (e16Solve) where

import Data.Char (digitToInt)

e16Solve:: Int
e16Solve = sum . map digitToInt . show $ 2 ^ 1000
