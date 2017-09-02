{--
  https://projecteuler.net/problem=33

  let n is numerator, d is denomirator, i is deleted number.
  There are four pattern as below.
  
  1. (10i + n) / (10i + d) = n/d
  2. (10n + i) / (10d + i) = n/d
  3. (10i + n) / (10d + i) = n/d
  4. (10n + i) / (10i + d) = n/d
  
  (1 <= n, d <= 9, 1 <= i <= 9)

  1 and 2 is impossible, because d is not equal to n. In case of 3,

  (10i + n)d = (10d + i)n
  10di + nd = 10nd + ni
  9nd = 10di - ni
      = 10di + (di -di) - ni
      = 9di + i(d - n)
  9d(n - i) = i(d - n)

  d > n,  and right side is positive, since left side is positive. Therefore n > i.

  n - i = i/9 - in/pd

  Right side is less than 1 because of i < n <= 9. But left side is greater than 1.
  Therefore, 3 is impossible.
  
  So, We only have to consider about case 4.
--}

module Euler33 (e33_solve) where

import Control.Monad (guard)

e33_solve :: Integral a => a
e33_solve =
  let
    f = foldr1 (\(d, n) (a, b) -> (d * a, n * b)) cancellings
    d = fst f
    n = snd f
  in
    d `div` gcd d n

cancellings :: Integral a => [(a, a)]
cancellings = do
  i <- [1..9]
  d <- [1..i-1]
  n <- [1..d-1]
  guard (d * (n * 10 + i) ==  n * (i * 10 + d))
  return (d,n)
