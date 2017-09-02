{--
  https://projecteuler.net/problem=72

  It's sum of the totient function from 2 to 10 ^ 6.
--}

module Euler72 (e72_solve) where

import Math.NumberTheory.Primes.Factorisation (totientSieve, sieveTotient)

e72_solve :: Integer
e72_solve =
  let
    n = 10 ^ 6
    t = totientSieve n
  in
    sum . map (sieveTotient t) $ [2..n]
