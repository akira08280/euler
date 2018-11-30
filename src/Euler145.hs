{--
  https://projecteuler.net/problem=145

  Pen and Paper solution

  1digit -> 0
  2digit -> 20
  3digit -> 100
  4digit -> 600
  5digit -> 0
  6digit -> 18000
  7digit -> 50000
  8digit -> 540000
  9digit -> 0
--}

module Euler145 (e145Solve) where

e145Solve :: Integral a => a
e145Solve = sum [0,20,100,600,0,18000,50000,540000,0]
