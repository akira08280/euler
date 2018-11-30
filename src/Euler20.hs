{--
  https://projecteuler.net/problem=20
--}

module Euler20 (e20Solve) where

import Data.Char (digitToInt)

e20Solve :: Int
e20Solve = sum . map digitToInt . show $ product [1..100]
