{--
  https://projecteuler.net/problem=16
--}

module Euler16 (e16_solve) where

import Data.Char (digitToInt)

e16_solve:: Int
e16_solve = sum . map digitToInt . show $ 2 ^ 1000
