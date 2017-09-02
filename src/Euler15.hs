{--
  https://projecteuler.net/problem=15

  For example, all ways in 2 * 2 grid as below.

  down  -> down  -> right -> right
  down  -> right -> down  -> right
  down  -> right -> right -> down
  right -> down  -> down  -> right
  right -> down  -> right -> down
  right -> right -> down  -> down

  This is two downs and two rights combination.
  So, It is equal to problem setting two rights from four.
--}

module Euler15 (e15_solve) where

e15_solve :: Integral a => a
e15_solve = c 40 20

c :: Integral a => a -> a -> a
c n k = f n k `div` f k k
  where
    f n k = product [n - k + 1..n]
