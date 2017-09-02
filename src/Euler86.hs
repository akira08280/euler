{--
  https://projecteuler.net/problem=86
   ______________________
  |  |                |  |
  |__|________________|__|
     |                |    S
     |                |
     |________________|

             M

  The condition of shortest path is that sqrt (M^2 + S^2) is integer.
  Range of S have to be less than 2 * M, because the either side dividing S exceeded M.

  In case of M >= S, we have to divide S to the number of (S / 2).
  For example, when M = 10 and S = {10,9}.
  
  S=10  S=9
 
  1,9   1,8
  2,8   2,7
  3,7   3,6
  4,6   4,5
  5,5   ---
  ---   5,4
  4,6   4,5 <- duplicate

  In case of M < S, we have to divide S to the number of M - (S - 1) / 2.
  For example, when M = 10 and S = {16,14,14,13}.
  
  S=16  S=15  S=14  S=13
  10-7  10-7  10-6  10-6

  1,15  1,14  1,13  1,12
  2,14  2,13  2,12  2,11
  3,13  3,12  3,11  ----
  4,12  4,11  ----  3,10
  5,11  ----  4,10  4,9
  ----  5,10  5,9   5,8
  6,10  6,9   6,8   6,7
  7,9   7,8   7,7   ----
  8,8   ----  ----  7,6
  ----  8,7   8,6   8,5
  9,7   9,6   9,5   9,4 <- duplicate
--}

module Euler86 (e86_solve) where

import Control.Monad (guard)
import Data.Maybe (fromJust)
import Data.List (findIndex)

e86_solve :: Int
e86_solve = fromJust . findIndex (> 10 ^ 6) . scanl1 (+) . map (sum . shortest) $ [0..]

shortest :: Integral a => a -> [a]
shortest m = do
  s <- [1..2 * m]
  guard (isSquare (m ^ 2 + s ^ 2))
  if m >= s then
    return (s `div` 2)
  else
    return (m - (s - 1) `div` 2)

isSquare :: Integral a => a -> Bool
isSquare x = (== x) . (^ 2) . truncate . sqrt . fromIntegral $ x
