{--
  https://projecteuler.net/problem=123

  If n is odd, yhe Reminder of (a-1)^n + (a+1)^n is (2 * n * p) is 2*n*p.
    -> Problem 120 solution
--}

module Euler123 (e123_solve) where

import Data.Numbers.Primes (primes)

e123_solve :: Integer
e123_solve = fst . head . dropWhile (\(n,p) -> 2*n*p < 10^10) . filter (odd . fst) . zip [1..] $ primes
