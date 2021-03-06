{--
  https://projecteuler.net/problem=133
--}

module Euler133 (e133Solve) where

import Data.Numbers.Primes (primes)
import Math.NumberTheory.Powers.Modular (powMod)

e133Solve :: Integer
e133Solve = (+ 5) . sum . filter (not . isPrimeFactor) . takeWhile (< limit) . dropWhile (< 5) $ primes

isPrimeFactor :: Integral a => a -> Bool
isPrimeFactor = (== 1) . powMod 10 (10 ^ 50)

limit :: Integral a => a
limit = 10 ^ 5
