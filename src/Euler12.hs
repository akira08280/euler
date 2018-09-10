{--
  https://projecteuler.net/problem=12

  k's triangle number is k * (k + 1) / 2.
  Positive integer k and k + i is coprime.
  If k is even, k + 1 is odd, then k/2 and k + 1 is coprime.
  Similarly, if k is odd, k and (k + 1) /2 is coprime.
  
  If f(x) is function to get divisor of positve integer and a and b is coprime, f(a * b) = f(a) * f(b).
  In case k is even, f(k / 2) * f(k + 1) is divisors count, in case k is odd, f(k) * f((k + 1) / 2) is divisors count.

  thanks to :
  http://tsumuji.cocolog-nifty.com/tsumuji/2010/01/project-euler-2.html
--}

module Euler12 (e12_solve) where

import NumberTheory (divisors)

e12_solve :: Int
e12_solve = countDivisorTriangle 1 1 500

countDivisorTriangle :: Int -> Int -> Int -> Int
countDivisorTriangle d1 n lim
  | d1 * d2 >= lim = (n - 1) * n `div` 2
  | otherwise = countDivisorTriangle d2 (succ n) lim
  where
    d2 = length . divisors $ (if even n then n `div` 2 else n)
