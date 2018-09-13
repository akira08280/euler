{--
  https://projecteuler.net/problem=137
  
  Nth Golden nugget's formula is fib(2 * n) * fib(2 * n + 1)
  https://oeis.org/A081018
--}

module Euler137 (e137_solve) where

e137_solve :: Integral a => a
e137_solve = fib (nth * 2) * fib (nth * 2 + 1)

fib :: Integral a => a -> a
fib n = round $ phi ** fromIntegral n / sq5
  where
    sq5 = sqrt 5 :: Double
    phi = (1 + sq5) / 2

nth :: Integral a => a
nth = 15
