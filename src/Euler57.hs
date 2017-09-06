{--
  https://projecteuler.net/problem=57

  first four time repeat

  1 + 1/2 = 3/2 = 1.5
  1 + 1/(2 + 1/2) = 7/5 = 1.4
  1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...
  1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...

  iterate continuedFraction $ (1 % 2) -> 1/(2 + 1/(2 + 1/(2 + 1/2)))
--}

module Euler57 (e57_solve) where

import Data.Ratio ((%), numerator, denominator, Ratio)
import Common (digit)

e57_solve :: Int
e57_solve = length . filter comp . take (10 ^ 3) . map succ . iterate continuedFraction $ (1 % 2)

continuedFraction :: Integral a => Ratio a -> Ratio a
continuedFraction x = 1 / (2 + x)

comp :: Ratio Integer -> Bool
comp f = digit (denominator f) < digit (numerator f)
