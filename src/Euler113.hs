{--
  https://projecteuler.net/problem=113
--}

module Euler113 (e113_solve) where

import Common (c)

e113_solve :: Integral a => a
e113_solve = sum . map bounds $ [1..100]

bounds :: Integral a => a -> a
bounds d = increasingNumbers + decreasingNumbers - allZero - duplicateNumbers
  where
    increasingNumbers = c (d + 8) d
    decreasingNumbers = c (d + 9) d
    duplicateNumbers = 9
    allZero = 1
