{--
  https://projecteuler.net/problem=114
--}

module Euler114 (e114Solve) where

import Block (flexBlockWays)

e114Solve :: Integer
e114Solve = last . flexBlockWays 3 $ 50
