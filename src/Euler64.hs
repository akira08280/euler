{--
  https://projecteuler.net/problem=64

  algorithm:
  [https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Algorithm]
--}

module Euler64 (e64_solve) where

import Common (third, isSquare)

e64_solve :: Int
e64_solve = length . filter (odd . length . continuedFraction) $ [1..10 ^ 4]

continuedFraction :: Integral a => a -> [(a, a, a)]
continuedFraction s
  | isSquare s = []
  | otherwise = takeWhile ((/= 2 * a0) . third) . iterate f $ (0, 1, a0)
  where
    a0 = floor . sqrt . fromIntegral $ s
    f (m, d, a) = (m', d', a')
      where
        m' = d * a - m
        d' = (s - m' ^ 2) `div` d
        a' = floor $ (fromIntegral (a0 + m')) / (fromIntegral d')
