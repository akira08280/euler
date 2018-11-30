{--
  https://projecteuler.net/problem=148

  This is the best problem I've ever experienced.
  I'm going to introduce how to solve as soon as possible easily.
--}

module Euler148 (e148Solve) where

e148Solve :: Integer
e148Solve = f . nBaseArray . pred $ upper

f :: [Integer] -> Integer
f []     = 0
f [m]    = triangle . succ $ m
f (m:ms) = triangle m * triangle base ^ length ms + succ m * f ms

triangle :: Integral a => a -> a
triangle n = n * (n + 1) `div` 2

nBaseArray :: Integer -> [Integer]
nBaseArray 0 = []
nBaseArray n = nBaseArray q ++ [m]
  where
    (q, m) = n `divMod` base

base :: Integer
base = 7

upper :: Integer
upper = 10 ^ 9
