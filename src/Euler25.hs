{--
  https://projecteuler.net/problem=25
--}

module Euler25 (e25_solve) where

import Data.Char (digitToInt)
import NumberTheory (fibonacci)

e25_solve :: Int
e25_solve = length . takeWhile ((< 10 ^ 3) . length . map digitToInt . show) $ fibonacci
