{--
  https://projecteuler.net/problem=39

  There are 2 formulas. 1 is the Pytagorean theorem. 2 is the sum of 3 sides of right triangle.

  1. a ^ 2 + b ^ 2 = c ^ 2
  2. a + b + c = p

  If both a and b are even, c is even. For that reason, p is even from 2.
  If either a or b are odd, c is odd. For that reason, p is even from 2.
  If both a and b are odd, c is even. For that reason, p is even from 2.
  So p is even. Besides, 1 and 2 yields below equation. 
  
  a ^ 2 + b ^ 2 = (p - a - b) ^ 2
                = p ^ 2 + a ^ 2 + b ^ 2 - 2pa - 2pb + 2ab
              b = (p ^ 2 - 2pa) / (2p - 2a)

  The range of a is p / 3, because of a <= b < c and 2.
--}

module Euler39 (e39_solve) where

import Control.Monad (guard)
import Data.Ord (comparing)
import Data.List (sort, group, maximumBy)

e39_solve :: Integral a => a
e39_solve = fst . maximumBy (comparing snd) . map (\all@(x:_) -> (x, length all)) . group . sort $ triangles

triangles :: Integral a => [a]
triangles = do
  p <- [2,4..10 ^ 3]
  a <- [2..p `div` 3]
  guard (p * (p - 2 * a) `mod` (2 * (p - a)) == 0)
  return p
