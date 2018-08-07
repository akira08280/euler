{--
  https://projecteuler.net/problem=114
--}

module Euler114 (e114_solve) where

import Block (flexBlockWays)

e114_solve :: Integer
e114_solve = last . flexBlockWays 3 $ 50
