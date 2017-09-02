{--
  https://projecteuler.net/problem=1
--}

module Euler1 (e1_solve) where

import Control.Monad (guard)

e1_solve :: Integral a => a
e1_solve = sum multiples

multiples :: Integral a => [a]
multiples = do
  a <- [1..999]
  guard (a `mod` 3 == 0 || a `mod` 5 == 0)
  return a
