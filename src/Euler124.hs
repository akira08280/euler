{--
  https://projecteuler.net/problem=124
--}

module Euler124 (e124_solve) where

import Common (sortedRads)

e124_solve :: Integer
e124_solve = fst $ sortedRads limit !! 9999

limit :: Integral a => a
limit = 10 ^ 5
