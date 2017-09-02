{--
  https://projecteuler.net/problem=26
--}

module Euler26 (e26_solve) where

import Data.Ord (comparing)
import Data.List (maximumBy)
import qualified Data.Set as Set (Set, empty, insert, member, size)

e26_solve :: Int
e26_solve = fst . maximumBy (comparing snd) . cycles $ 10 ^ 3

cycles ::Integral a => a -> [(a, Int)]
cycles n = do
  a <- [2..n]
  let
    c = count a 1 Set.empty
  return (a, c)

count :: Integral a => a -> a -> Set.Set a -> Int
count n v s
  | v == 0 = Set.size s
  | Set.member v s = Set.size s
  | otherwise = count n r (Set.insert v s)
  where
    r = v * 10 `mod` n
