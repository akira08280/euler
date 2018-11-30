{--
  https://projecteuler.net/problem=301
--}

module Euler301 (e301Solve) where

import Data.Bits (xor)

e301Solve :: Int
e301Solve = length . filter nim $ [1..2 ^ 30]

nim :: Int -> Bool
nim n = n `xor` n * 2 `xor` n * 3 == 0
