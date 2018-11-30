{--
  https://projecteuler.net/problem=94

  Define the adge of triangle as (a, a, b), and a = b +- 1.
  Let the area of the triangle is T.

  By Helon's formula. [https://en.wikipedia.org/wiki/Heron%27s_formula]
  T = b / 2 * sqrt (a ^ 2 - 1 / 4 * b ^ 2)

  Therefore b is even, so let b = 2n, a = 2n +- 1.
  Then, area T is

  T = n * sqrt (3n ^ 2 +- 4n - 1)

  For T to be an integer,

  k ^ 2 = 3n ^ 2 +- 4n - 1
  (3n +- 2) ^ 2 - 3k ^ 2 = 1

  This is the Pell's equatation. (x = 3n +- 2)
  From the problem 66, the minimum solution when n = 3 is x = 2 y = 1.
  Therefore, the following recurrence formula holds.[https://en.wikipedia.org/wiki/Pell%27s_equation#Solutions]

  z(k + 1) = 2(xk) + 3(yk)
  y(k + 1) = x(k) + 2y(k)

  Since a = 2n +- 1,
  b = 2 the length of the surrounding side is 6n +- 2.

  When x = 3n + 2, that is x is 2 (mod 3).

  2x     = 6n + 4
  2x - 2 = 6n + 2 <- (the length of surrounding side)

  When x = 3n - 2, that is x is 1 (mod 3).

  2x     = 6n - 4
  2x + 2 = 6n - 2 <- (the length of surrounding side)
--}

module Euler94 (e94Solve) where

import Common (third)

e94Solve :: Int
e94Solve = sum . takeWhile (< 10 ^ 9) . map third . iterate next $ (2, 1, 0)

next :: Integral a => (a, a, a) -> (a, a, a)
next (x, y, _) = (x', y', length)
  where
    x' = 2 * x + 3 * y
    y' = x + 2 * y
    length
      | x `mod` 3 == 2 = 2 * x' - 2
      | otherwise = 2 * x' + 2
