{--
  https://projecteuler.net/problem=127

  The radical of n, rad(n), is the product of distinct prime factors of n.
  For example, 504 = 23 x 32 x 7, so rad(504) = 2 x 3 x 7 = 42.
  We shall define the triplet of positive integers (a, b, c) to be an abc-hit if:

  1. GCD(a, b) = GCD(a, c) = GCD(b, c) = 1
  2. a < b
  3. a + b = c
  4. rad(abc) < c

  Insight (1)

    We have defined the following relationship a + b = c.
    Then If and only if GCD(a,b) = 1 the it follows that GCD(a, c) = GCD(b, c) = 1.
    GCD(a, c) = GCD(a, a + b)
    This means (a + b) / d = a / d + b / d. But since GCD(a, b) = 1, there is no d which divides a and b.
    So we only need to check GCD(a, b).

  Insight (2)

    If GCD(a, b) = 1 then we have that rad(a) * rad(b) * rad(c) = rad(abc).
    Because GCD(a, b) = GCD(a, c) = GCD(b, c) = 1, so a, b and c do not have a common divisor.

  Insight (3)

    Since a < b, rad(b) > 2.
    Since rad(a) * rad(b) * rad(c) < c, rad(a) * rad(c) < c / 2.
    Therefore, you do not need to check the following cases.
      -> rad(a) * rad(c) >= c / 2

  Insight (4)

    Since a + b = c and a < b, a < c / 2.
--}

module Euler127 (e127_solve) where

import Rads (sieve)
import Control.Monad (guard)
import Data.Ord (comparing)
import qualified Data.Vector.Unboxed as U ((!), toList, modify)
import qualified Data.Vector.Algorithms.Intro as I (sortBy)

e127_solve :: Int
e127_solve = sum abchits

abchits :: [Int]
abchits = do
  c <- [3..limit]
  let
    radc = snd $ vRads U.! c
    halfc = c `div` 2
  t <- takeWhile (\rad -> let rada = snd rad in rada * radc <= halfc) sortedRads
  let
    a = fst t
  guard (a < halfc)
  let
    b = c - a
    rada = snd t
    radb = snd $ vRads U.! b
  guard (rada * radb * radc < c)
  guard (gcd a b == 1)
  return c
  where
    vRads = sieve limit
    sortedRads = U.toList . U.modify (I.sortBy $ comparing snd) $ vRads

limit :: Integral a => a
limit = 12 * 10 ^ 4
