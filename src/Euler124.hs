{--
  https://projecteuler.net/problem=124
--}

module Euler124 (e124_solve) where

import Common (rads)
import Data.List (sortBy)
import Data.Ord (comparing)

e124_solve :: Integer
e124_solve = fst $ (sortBy (comparing snd) . rads $ limit) !! 9999

limit :: Integral a => a
limit = 10 ^ 5
