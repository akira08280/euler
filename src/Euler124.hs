{--
  https://projecteuler.net/problem=124
--}

module Euler124 (e124Solve) where

import Rads (list)
import Data.List (sortBy)
import Data.Ord (comparing)

e124Solve :: Int
e124Solve = fst $ (sortBy (comparing snd) . list $ limit) !! 10000

limit :: Integral a => a
limit = 10 ^ 5
