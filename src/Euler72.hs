{--
  https://projecteuler.net/problem=72

  It's sum of the totient function from 2 to 10 ^ 6.
--}

module Euler72 (e72Solve) where

import Math.NumberTheory.ArithmeticFunctions (totientA)
import Math.NumberTheory.ArithmeticFunctions.SieveBlock (runFunctionOverBlock)

e72Solve :: Word
e72Solve = sum . runFunctionOverBlock totientA 2 $ limit

limit :: Integral a => a
limit = 10 ^ 6 - 1
