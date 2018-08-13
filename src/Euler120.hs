{--
  https://projecteuler.net/problem=120

  n=1 : (a-1)   + (a+1)   = 2a
  n=2 : (a-1)^2 + (a+1)^2 = 2a^2 + 2
  n=3 : (a-1)^3 + (a+1)^3 = 2a^3 + 6a
  n=4 : (a-1)^4 + (a+1)^4 = 2a^4 + 12a^2 + 2
  n=5 : (a-1)^5 + (a+1)^5 = 2a^5 + 20a^3 + 10a

  The remainder obtained by dividing by a^2 is as follows.

  n=1 : 2a
  n=2 : 2
  n=3 : 6a
  n=4 : 2
  n=5 : 10a

  When n is an even number, the remainder becomes 2, so n is odd.(If n is 2, it can not be the maximum value.)
  The remainder from the above pattern is 2*n*a, and 2*n*a becomes the remainder maximum value of n^2 when 2*n = a-1.
  Because 2*n = a, since the remainder becomes n^2, it becomes 0.
  So n = (a-1)/2.
--}

module Euler120 (e120_solve) where

e120_solve :: Integer
e120_solve = sum . map (\a -> 2 * a * ((a - 1) `div` 2)) $ [3..1000]
