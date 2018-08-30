{--
  https://projecteuler.net/problem=127
--}

module Euler127 (e127_solve) where

import Common (rads)
import Control.Monad (guard)
import Data.Ord (comparing)
import Data.List (nub)
import qualified Data.Vector as V ((!), Vector, fromList, modify, takeWhile, toList, filter)
import qualified Data.Vector.Algorithms.Intro as I (sortBy)

e127_solve :: Integer
e127_solve = sum abchits

abchits :: [Integer]
abchits = do
  c <- [3..limit]
  let
    radc = snd $ vrads V.! (fromIntegral $ c - 1)
    halfc = c `div` 2
  t <- V.toList .
       V.filter (\rad -> let a = fst rad in a < halfc) .
       V.takeWhile (\rad -> let rada = snd rad in rada * radc <= halfc) $ vsortedRads
  let
    a = fst t
    b = c - a
    rada = snd t
    radb = snd $ vrads V.! (fromIntegral $ b - 1)
  guard (rada * radb * radc < c)
  guard (gcd rada radb == 1)
  return c
  where
    vrads = V.fromList . rads $ limit
    vsortedRads = vecSort (comparing snd) vrads

vecSort :: (a -> a -> Ordering) -> V.Vector a -> V.Vector a
vecSort cmp = V.modify (I.sortBy cmp)

limit :: Integral a => a
limit = 12 * 10 ^ 4
