{--
  https://projecteuler.net/problem=91
--}

module Euler91 (e91Solve) where

e91Solve :: Integral a => a
e91Solve =
  let
    size   = 5 * 10 ^ 1
    xaxis  = size * size
    yaxis  = size * size
    origin = size * size
    others = sum . others' $ size
  in
    sum [xaxis, yaxis, origin, others]

others' :: Integral a => a -> [a]
others' size = do
  x <- [1..size]
  y <- [1..size]
  let
    g = gcd x y
    slopeX = x `div` g
    slopeY = y `div` g
    downwords = min ((size - x) `div` slopeY) (y `div` slopeX)
  return (downwords * 2) -- double to contain upward triangles symmetric about the line y = x.
