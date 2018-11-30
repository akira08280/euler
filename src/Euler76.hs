{--
  https://projecteuler.net/problem=76

  origin
  https://wiki.haskell.org/Euler_problems/31_to_40#Problem_31

  It is possible to write five as a sum in exactly six different ways:

  4 + 1
  3 + 2
  3 + 1 + 1
  2 + 2 + 1
  2 + 1 + 1 + 1
  1 + 1 + 1 + 1 + 1

  How many different ways can one hundred be written as a sum of at least two positive integers?

  this problem is same as the way to solve using dynamic programming algorithm like problem 31.
  All we have to care is that oneself is not contained, like problem statement.
  For that reason, we have to subtract 1 at last.
--}

module Euler76 (e76Solve) where

e76Solve :: Integral a => a
e76Solve = pred . sumofways $ [1..100] -- subtract 1 to except in the case of only 100

sumofways :: Integral a => [Int] -> a
sumofways xs = ways xs !! length xs
  where
    ways [] = 1 : repeat 0
    ways (coin:coins) = n 
      where
        n = zipWith (+) (ways coins) (replicate coin 0 ++ n)
