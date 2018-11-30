{--
  https://projecteuler.net/problem=100

  Let b is number of blue discs, and let n is total number of discs. 
  
  b   b - 1   1
  - * ----- = -
  n   n - 1   2

  b ^ 2 - b   1
  --------- = -
  n ^ 2 - n   2

  2b ^ 2 - 2b
  ----------- = 1
  n ^ 2 - n

  2b ^ 2 - b = n ^ 2 - n
  2b ^ 2 - b - n ^ 2 + n = 0

  This is a Diophantine Quadratic Equation.
  We can solve it on this site. [https://www.alpertron.com.ar/QUAD.HTM]

  X0 = 1
  Y0 = 1

  Xn+1 = 3Xn + 2Yn – 2
  Yn+1 = 4Xn + 3Yn – 3
--}

module Euler100 (e100Solve) where

e100Solve :: Integral a => a
e100Solve = fst . head . dropWhile ((< 10 ^ 12) . snd) . iterate next $ (1, 1)

next :: Integral a => (a, a) -> (a, a)
next (b, n) = (3 * b + 2 * n - 2, 4 * b + 3 * n - 3)
