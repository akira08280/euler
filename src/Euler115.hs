{--
  https://projecteuler.net/problem=115
--}

module Euler115 (e115Solve) where

import Block (flexBlockWays)

e115Solve :: Int
e115Solve = length . takeWhile (< 1000000) . flexBlockWays 50 $ 200
