{--
  https://projecteuler.net/problem=134

  For example, in the case of 19 and 23.
  Divide by 100 to find the reminder 19 and the number divisible by 23 with the Chinese Remainder Theorem.

  1219 = 0  (mod 23)
  1219 = 19 (mod 100)
--}

module Euler134 (e134Solve) where

import Common (digit)
import Math.NumberTheory.Moduli.Chinese (chineseRemainder2)
import Math.NumberTheory.Primes.Sieve (primeList, primeSieve)

e134Solve :: Integer
e134Solve = sum . map smallest $ [2..(length primes - 2)]
  where
    primes = sieve $ limit + 4 -- last prime's pair is (999983, 1000003)
    smallest n = chineseRemainder2 (p1, 10 ^ d1) (0, p2)
      where
        p1 = primes !! n
        p2 = primes !! succ n
        d1 = digit p1

sieve :: Integer -> [Integer]
sieve = primeList . primeSieve

limit :: Integral a => a
limit = 10 ^ 6
