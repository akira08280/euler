{--
  https://projecteuler.net/problem=58
--}

module Euler58 (e58Solve) where

import Data.Ratio ((%))
import MillerRabin (isPrime)

e58Solve :: Integer
e58Solve = edges 0 2 1

edges :: Int -> Integer -> Integer -> Integer
edges p e s
  | rto < 1 % 10 = e - 1
  | otherwise = edges p' (e + 2) (succ s)
  where
    s' = (2 * s - 1) ^ 2
    p' = p + (length . filter isPrime $ [e + s', 2 * e + s', 3 * e + s'])
    rto = p' % (fromIntegral (2 * e) :: Int)
