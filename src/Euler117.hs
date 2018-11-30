{--
  https://projecteuler.net/problem=117

  a(n) = a(n-1) + a(n-2) + a(n-3) + a(n-4)
--}

module Euler117 (e117Solve) where

e117Solve :: Integer
e117Solve = last . take 51 . map (\(_, _, _, d) -> d) . iterate loop $ (0, 0, 0, 1)
  where
    loop (a, b, c, d) = (b, c, d, a + b + c + d)
