{--
  https://projecteuler.net/problem=25
--}

module Euler25 (e25Solve) where

import Data.Char (digitToInt)
import NumberTheory (fibonacci)

e25Solve :: Int
e25Solve = length . takeWhile ((< 10 ^ 3) . length . map digitToInt . show) $ fibonacci
