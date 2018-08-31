{--
  https://projecteuler.net/problem=124
--}

module Euler124 (e124_solve) where

import Rads (list)
import Data.List (sortBy)
import Data.Ord (comparing)

e124_solve :: Int
e124_solve = fst $ (sortBy (comparing snd) . list $ limit) !! 10000

limit :: Integral a => a
limit = 10 ^ 5
