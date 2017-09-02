{--
  https://projecteuler.net/problem=73

  Faray sequence
  [https://en.wikipedia.org/wiki/Farey_sequence#Next_term]
  
  1 % 3 < 4000 % 11999 < p % q,,,,1 % 2
  
	A result by repeating calculation until denominator exceeding 12000, we will find 4000 % 11999 which is the right of 1 % 3. (ref euler 71)
  To find answer, repeating calculation until that next term will become 1%2 using 1%3 and 4000 % 11999.

  a % b < c % d < p % q,,,,,,,,,,,

  (a + p) % (b + q) = c % d

  Since c % d is in lowest terms, there must be an integer k such that kc = a + p and kd = b + q, giving
  p = kc - a and q = kd - b. If we consider p and q to be functions of k, then

  p(k)   c   p(k)d - cq(k)   (kc - a)d - (kd - b)c   kdc - ad - kdc + bc   bc - ad     1
  ---- - - = ------------- = --------------------- = ------------------- = --------- = --
  q(k)   d   q(k)d           d(kd - b)               d(kd - b)             d(kd - b)   dq

  Above expression means, larger k gets, the closer p % q gets to c % d.
  To give the next term in the sequence k must be as large as possible,
  subject to kd - b <= n (as we are only considering numbers with denominators not greater than n), so k is the greatest integer <= (n + b) % d.

  a = 1
  b = 3
  c = 4000 is next a
  d = 11999 is next b
  p = int((12000 + 3) % 11999) * 4000  - 1 is next c.
  q = int((12000 + 3) % 11999) * 11999 - 3 is next d.
--}

module Euler73 (e73_solve) where

e73_solve :: Int
e73_solve = length . takeWhile (not . (\(_, _, c, d) -> (c == 1 && d == 2))) . iterate f $ (1,3,4000,11999)

f :: Integral a => (a, a, a, a) -> (a, a, a, a)
f (a, b, c, d) = (c, d, k * c - a, k * d - b)
  where
    n = 12000
    k = (n + b) `div` d
