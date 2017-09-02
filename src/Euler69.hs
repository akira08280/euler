{--
  https://projecteuler.net/problem=69

  {p1,p2,p3,p4..} is prime factor of n.
  
  phy(n)     = n * (1 - 1 % p1) * (1 - 1 % p2) * (1 - 1 % p3) * (1 - 1 * p4) ...
  n / phy(n) = n % (n * (1 - 1 % p1) * (1 - 1 % p2) * (1 - 1 % p3) - (1 - 1 % p4)) ...
             = 1 % ((1 - 1 % p1) * (1 - 1 % p2) * (1 - 1 % p3) - (1 - 1 % p4)) ...
  
  We want to find n for which n / phy(n) is a maximum. (n <= 1000000)
  As demonstrated above, denominator has to be as small as possible,
  so number of distinct prime factors of n should be a lot.
  Moreover, p would be better off small, because (1 - 1 % p) becomes smaller.
--}

module Euler69 (e69_solve) where

import Data.Numbers.Primes (primes)

e69_solve :: Integral a => a
e69_solve = last . takeWhile (<= 10 ^ 6) . scanl1 (*) $ primes
