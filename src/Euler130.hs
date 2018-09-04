{--
  https://projecteuler.net/problem=130

  Due to the nature of A(n), R(A(n)) is divisible by n.
  If (n - 1) is divisible by A(n), R(n - 1) can also be divisible by n.
  Because n - 1 is larger than A(n) and R(n - 1) is multiple of R(A(n)), R(A(n)) is divisible by n. Therefore R(n - 1) is divisible by n.

  -- Repunit Wikipedia
    https://en.wikipedia.org/wiki/Repunit#Properties

  -- Exanple
    R(35) is divisible by R(7), because 35 is divisible by 7.
    11111111111111111111111111111111111 `divMod` 1111111 = (10000001000000100000010000001, 0)

  R(n - 1) = (10^(n - 1) - 1) / 9.

  Since R(n - 1) can also be divisible by n, (10^(n - 1) - 1) / 9n.
  So 10^(n - 1) = 1 (mod 9n)
--}

module Euler130 (e130_solve) where

import MillerRabin (isPrime)

e130_solve :: Integer
e130_solve = sum . take limit . filter checkRep . filter (not . isPrime) $ [91, 93..]

checkRep :: Integral a => a -> Bool
checkRep n = 10 ^ (n - 1) `mod` (9 * n) == 1

limit :: Integral a => a
limit = 25
