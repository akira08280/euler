{--
  https://projecteuler.net/problem=24
--}

module Euler24 (e24_solve) where

import Data.Digits (unDigits)
import Common (permute)

e24_solve :: Int
e24_solve = unDigits 10 $ permute [0..9] !! (pred $ 10 ^ 6)
