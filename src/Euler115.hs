{--
  https://projecteuler.net/problem=115
--}

module Euler115 (e115_solve) where

import Block (ways)

e115_solve :: Int
e115_solve = length . takeWhile (< 1000000) . ways 50 $ 200
