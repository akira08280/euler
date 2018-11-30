{--
  https://projecteuler.net/problem=191

     0A 1A 2A
  1L  A  B  C
  0L  D  E  F

                         0A 1A 2A
  1L  A + B + C + D + E + F  A  B
  0L              D + E + F  D  E
--}

module Euler191 (e191Solve) where

e191Solve :: Integer
e191Solve = last . take 30 . map _sum . iterate next $ (1, 0, 0, 1, 1, 0) -- Initially it can only be 0, 1A or 1L

next :: Integral a => (a, a, a, a, a, a) -> (a, a, a, a, a, a)
next (a, b, c, d, e, f) = (_a, _b, _c, _d, _e, _f)
  where
    _a = a + b + c + d + e + f
    _b = a
    _c = b
    _d = d + e + f
    _e = d
    _f = e

_sum :: Integral a => (a, a, a, a, a, a) -> a
_sum (a, b, c, d, e, f) = a + b + c + d + e + f
