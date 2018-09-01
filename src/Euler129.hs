{--
  https://projecteuler.net/problem=129

  Since A(n) < n, so we can start n from 1000000.
  And we can test only odd numbers because gcd (n, 10) == 1.
--}

module Euler129 (e129_solve) where

import Repunit (a)

e129_solve :: Int
e129_solve = head . filter (\n -> a n > limit) $ [limit + 1, limit + 3..]

limit :: Integral a => a
limit = 10 ^ 6
