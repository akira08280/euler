{--
  https://projecteuler.net/problem=148

  This is the best problem I've ever experienced.
  I'm going to introduce how to solve as soon as possible.
--}

module Euler148 (e148_solve) where

e148_solve :: Integer
e148_solve = f . nBaseArray $ upper

f :: [Integer] -> Integer
f []     = 0
f [m]    = triangle . succ $ m
f (d:ms) = triangle d * triangle base ^ length ms + (d + 1) * f ms

triangle :: Integral a => a -> a
triangle n = n * (n + 1) `div` 2

nBaseArray :: Integer -> [Integer]
nBaseArray n = nBaseArray' n []
  where
    nBaseArray' n' ms
      | q == 0 = m:ms
      | otherwise = nBaseArray' q (m:ms)
      where
        (q, m) = divMod n' base

base :: Integer
base = 7

upper :: Integer
upper = 10 ^ 9 - 1
