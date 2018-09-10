{--
  https://projecteuler.net/problem=41

  In pandigital number of from 4 digits to 9 digits, The numbers of 4 or 7 digits is candidate of primes.
  Because if sum of digits is divisible by 3, the number is multiple of 3.

  1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 = 45 (% 3 = 0) 
  1 + 2 + 3 + 4 + 5 + 6 + 7 + 8     = 36 (% 3 = 0)
  1 + 2 + 3 + 4 + 5 + 6 + 7         = 29 (% 3 = 2)
  1 + 2 + 3 + 4 + 5 + 6             = 21 (% 3 = 0)
  1 + 2 + 3 + 4 + 5                 = 15 (% 3 = 0)
  1 + 2 + 3 + 4                     = 10 (% 3 = 1)
--}

module Euler41 (e41_solve) where

import Data.Numbers.Primes (isPrime)
import Common (digit, isPandigital)

e41_solve :: Int
e41_solve = head . filter (`isPandigital` [1..7]) . filter isPrime $ [7654321,7654319..1234567]
