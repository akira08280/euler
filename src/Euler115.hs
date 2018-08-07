{--
  https://projecteuler.net/problem=115
--}

module Euler115 (e115_solve) where

import Block (flexBlockWays)

e115_solve :: Int
e115_solve = length . takeWhile (< 1000000) . flexBlockWays 50 $ 200
