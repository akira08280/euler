{--
  https://projecteuler.net/problem=138

  x^2 + h^2 = L^2
  x^2 + (2x +- 1)^2 = L^2 (h = 2x +- 1)
  x^2 + 4x^2 +- 4x + 1 = L^2
  5x^2 +- 4x + 1 = L^2

  X0 = 0
  Y0 = 1

  Xn+1 = -9Xn  - 4Yn - 4
  Yn+1 = -20Xn - 9Yn - 8

  http://www.alpertron.com.ar/QUAD.HTM
--}

module Euler138 (e138_solve) where

e138_solve :: Integral a => a
e138_solve = sum . map (abs . snd) . tail . take (succ limit) . iterate nextXY $ (0, 1)

nextXY :: Integral a => (a, a) -> (a, a)
nextXY (x, y) = (xn, yn)
  where
    xn = -9 * x - 4 * y - 4
    yn = -20 * x - 9 * y - 8

limit :: Integral a => a
limit = 12
