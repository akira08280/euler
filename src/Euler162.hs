{--
  https://projecteuler.net/problem=162
--}

module Euler162 (e162Solve) where

import Text.Printf (printf, PrintfType)

e162Solve :: PrintfType t => t
e162Solve = printf "%X\n" (sum . atLeastZeroOneA $ 16 :: Integer)

-- inclusion-exclusion principle
atLeastZeroOneA :: Integral a => a -> [a]
atLeastZeroOneA upperDigit = do
  digit <- [3..upperDigit]
  let
    whole = 15 * 16 ^ pred digit
    noneZero = 15 ^ digit
    noneOne = 14 * 15 ^ pred digit
    noneA = noneOne
    noneZeroOne = 14 ^ digit
    noneZeroA = noneZeroOne
    noneOneA = 13 * 14 ^ pred digit
    noneZeroOneA = 13 ^ digit
  return $ whole - (noneZero + noneOne + noneA) + (noneZeroOne + noneZeroA + noneOneA) - noneZeroOneA
