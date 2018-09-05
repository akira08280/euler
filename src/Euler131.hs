{--
  https://projecteuler.net/problem=131

  I referred to SMQ's comments.
  There is no way to solve this problem solving method.

  1)
  n ^ 3 + n ^ 2 * p = n * n * (n + p)
  (p is prime)

  In order for n * n * (n + p) is cubic number, n and (n + p) must be themselves cubic number.
  Since (n + p) - n = p, so p is difference of cubic numbers.

  2)
  a ^ 3 - b ^ 3 = (a - b)(a ^ 2 + a * b + b ^ 2)

  Since a ^ 3 - b ^ 3 must be prime, the difference a and b must be 1.
  If the difference a and b is larger than 1, (a - b) is larger than 1. This means that (a ^ 3 - b ^ 3) is multiple of (a - b).

  3)
  Since 1) and 2), a and b must be consective.
--}

module Euler131 (e131_solve) where

import MillerRabin (isPrime)

e131_solve :: Int
e131_solve = length . takeWhile (< limit) . filter isPrime . map diffConsecutiveCubes $ [1..]

diffConsecutiveCubes :: Integral a => a -> a
diffConsecutiveCubes n = (n + 1) ^ 3 - n ^ 3

limit :: Integral a => a
limit = 10 ^ 6
