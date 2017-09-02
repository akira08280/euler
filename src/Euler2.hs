{--
  https://projecteuler.net/problem=2
--}

module Euler2 (e2_solve) where

import NumberTheory (fibonacci)

e2_solve :: Integral a => a
e2_solve = sum . filter even . takeWhile (< 4 * 10 ^ 6) $ fibonacci
