{--
  https://projecteuler.net/problem=47
--}

module Euler47 (e47Solve) where

import NumberTheory (primes)

e47Solve :: Integral a => a
e47Solve = head . find $ 4

find :: Integral a => Int -> [a]
find n = find' (2 * 3 * 5 * 7) 1 []
  where
    find' a b c
      | b >= n = c
      | len == n = find' (succ a) (succ b) (c ++ [a])
      | otherwise = find' (succ a) 0 []
      where
        len = length . primes $ a
