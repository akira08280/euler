{--
  https://projecteuler.net/problem=20
--}

module Euler20 (e20_solve) where

import Data.Char (digitToInt)

e20_solve :: Int
e20_solve = sum . map digitToInt . show $ product [1..100]
