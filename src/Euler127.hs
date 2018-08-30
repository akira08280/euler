{--
  https://projecteuler.net/problem=127
--}

module Euler127 (e127_solve) where

import Common (rads)
import Control.Monad (guard)
import Data.Ord (comparing)
import qualified Data.Vector as V ((!), Vector, fromList, modify, takeWhile, toList)
import qualified Data.Vector.Algorithms.Intro as I (sortBy)

e127_solve :: Integer
e127_solve = sum abchits

abchits :: [Integer]
abchits = do
  c <- [3..limit]
  let
    radc = snd $ vRads V.! (fromIntegral $ c - 1)
    halfc = c `div` 2
  t <- takeWhile (\rad -> (snd rad) * radc <= halfc) $ sortedRads
  let
    a = fst t
  guard (a < halfc)
  let
    b = c - a
    rada = snd t
    radb = snd $ vRads V.! (fromIntegral $ b - 1)
  guard (rada * radb * radc < c)
  guard (gcd rada radb == 1)
  return c
  where
    vRads = V.fromList . rads $ limit
    sortedRads = V.toList . vecSort (comparing snd) $ vRads

vecSort :: (a -> a -> Ordering) -> V.Vector a -> V.Vector a
vecSort cmp = V.modify (I.sortBy cmp)

limit :: Integral a => a
limit = 12 * 10 ^ 4
