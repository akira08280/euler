{--
  https://projecteuler.net/problem=133
--}

module Euler133 (e133_solve) where

import Data.Numbers.Primes (primes)
import Math.NumberTheory.Powers.Modular (powMod)

e133_solve :: Integer
e133_solve = (+ 5) . sum . filter (not . isPrimeFactor) . takeWhile (< limit) . dropWhile (< 5) $ primes

isPrimeFactor :: Integral a => a -> Bool
isPrimeFactor p = (== 1) . powMod 10 (10 ^ 50) $ p

limit :: Integral a => a
limit = 10 ^ 5
