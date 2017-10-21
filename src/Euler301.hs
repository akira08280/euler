{--
  https://projecteuler.net/problem=301
--}

module Euler301 (e301_solve) where

import Data.Bits (xor)

e301_solve :: Int
e301_solve = length . filter nim $ [1..2 ^ 30]

-- nim :: Integral a => a -> Bool
nim :: Int -> Bool
nim n = n `xor` n * 2 `xor` n * 3 == 0
