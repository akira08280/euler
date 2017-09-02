{--
  https://projecteuler.net/problem=9
--}

module Euler9 (e9_solve) where

import Control.Monad (guard)

e9_solve :: Integral a => a
e9_solve = product . head $ triangles

triangles :: Integral a => [[a]]
triangles = do
  m <- [2..]
  n <- [1..m-1]
  let
    a = m ^ 2 - n ^ 2
    b = 2 * m * n
    c = m ^ 2 + n ^ 2
  guard (a + b + c == 10 ^ 3)
  return [a, b, c]
