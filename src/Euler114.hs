{--
  https://projecteuler.net/problem=114
--}

module Euler114 (e114_solve) where

import Block (ways)

e114_solve :: Integer
e114_solve = last . ways 3 $ 50
