{--
  https://projecteuler.net/problem=32

  9 digits pandigital number combination is 1 digit and 4 digits or 2 digits or 3 digits.
  
  1d * 4d = 4d (1 + 4 + 4 = 9)
  2d * 3d = 4d (2 + 3 + 4 = 9)
--}

module Euler32 (e32Solve) where

import Data.List (nub)
import Common (digit, isPandigital, concatIntArray)

e32Solve :: Int
e32Solve =
  let
    a = [a * b | a <- [1..9], b <- [1234..9876], let c = concatIntArray [a, b, a*b], isPandigital c [1..9]]
    b = [a * b | a <- [12..98], b <- [123..987], let c = concatIntArray [a, b, a*b], isPandigital c [1..9]]
  in
    sum . nub $ a ++ b
