{--
  https://projecteuler.net/problem=2
--}

module Euler2 (e2Solve) where

import NumberTheory (fibonacci)

e2Solve :: Integral a => a
e2Solve = sum . filter even . takeWhile (< 4 * 10 ^ 6) $ fibonacci
