{--
  https://projecteuler.net/problem=66

  Overview:
  https://en.wikipedia.org/wiki/Pell%27s_equation
    
    Pell's equation (also called the Pell-Fermat equation) is any of Diophantine equation of the form
    x ^ 2 - n * y ^ 2 = 1
    where n is a given positive non square integer and integer solutions are sought x and y.

  Solution:
  https://en.wikipedia.org/wiki/Pell%27s_equation#Fundamental_solution_via_continued_fractions

    Let hi/ki denote the sequence of convergents to the regular continued fraction for root n. 
    This seqence is unique. Then the pair (x1, y1) solving Pell's equation and minimizing x satisfies x1 = h1 and y1 = k1 for some i.

  algorithm:
  https://en.wikipedia.org/wiki/Continued_fraction#Some_useful_theorems
  https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Algorithm
--}

module Euler66 (e66_solve) where

import Data.Ord (comparing)
import Data.List (maximumBy)
import Common (isSquare)

e66_solve :: Integral a => a
e66_solve = 
  let
    a = filter (not . isSquare) [1..10 ^ 3]
  in
    fst . maximumBy (comparing snd) . zip a . map minimumPell $ a

minimumPell :: Integral a => a -> a
minimumPell s = fst .
                head .
                filter (\(x, y) -> x * x - s * y * y == 1) .
                map (\(_, _, _, h, _, k, _) -> (h, k)) .
                iterate f $ (0, 1, a0, a0, 1, 1, 0)
  where
    a0 = floor . sqrt . fromIntegral $ s
    f (m, d, a, h1, h2, k1, k2) = (m', d', a', h, h1, k, k1)
      where
        m' = d * a - m
        d' = (s - m' ^ 2) `div` d
        a' = floor $ (fromIntegral (a0 + m')) / (fromIntegral d')
        h  = a' * h1 + h2
        k  = a' * k1 + k2
