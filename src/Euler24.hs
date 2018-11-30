{--
  https://projecteuler.net/problem=24
--}

module Euler24 (e24Solve) where

import Data.Digits (unDigits)
import Common (permute)

e24Solve :: Int
e24Solve = unDigits 10 $ permute [0..9] !! pred (10 ^ 6)
