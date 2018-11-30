{--
  https://projecteuler.net/problem=101
--}

module Euler101 (e101Solve) where

import Data.List (intercalate)

e101Solve :: Integral a => a
e101Solve = sum . intercalate [] . takeWhile (not . null) . iterate diffNeighbor . map formula $ [1..10]

diffNeighbor :: Integral a => [a] -> [a]
diffNeighbor [] = []
diffNeighbor ns = zipWith (-) (tail ns) (init ns)

formula :: Integral a => a -> a
formula n = 1 - n + n^2 - n^3 + n^4 - n^5 + n^6 - n^7 + n^8 - n^9 + n^10
