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

import Common (c)

e15_solve :: Integral a => a
e15_solve = c 40 20
