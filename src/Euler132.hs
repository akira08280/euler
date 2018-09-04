{--
  https://projecteuler.net/problem=132

  When p is a prime factor of R(10^9), the following expression holds.

  10 ^ (10 ^ 9) - 1 / 9 = 0 (mod p)
  10 ^ (10 ^ 9) - 1 = 0 (mod p) 
  10 ^ (10 ^ 9) = 1 (mod p)

  Since 2 and 3 can not be prime factors, we check from prime numbers of 5 or more.
  2 is trivial, because 3 does not become a multiple of 3 even if adding 1 to 10^9.
--}

module Euler132 (e132_solve) where

import Data.Numbers.Primes (primes)
import Math.NumberTheory.Powers.Modular (powMod)

e132_solve :: Integer
e132_solve = sum . take limit . filter isPrimeFactor . dropWhile (< 5) $ primes

isPrimeFactor :: Integral a => a -> Bool
isPrimeFactor p = (== 1) . powMod 10 (10 ^ 9) $ p

limit :: Integral a => a
limit = 40
