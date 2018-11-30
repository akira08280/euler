{--
  https://projecteuler.net/problem=129

  Since A(n) < n, so we can start n from 1000000.
  And we can test only odd numbers because gcd (n, 10) == 1.
--}

module Euler129 (e129Solve) where

e129Solve :: Int
e129Solve = head . filter (\n -> a n > limit) $ [limit + 1, limit + 3..]

a :: Integral a => a -> a
a n
  | gcd n 10 > 1 = 0
  | otherwise = findDivisible 1 2
  where
    findDivisible m k
      | m' == 0 = k
      | otherwise = findDivisible m' (succ k)
      where
        m' = (m * 10 + 1) `mod` n -- Principle of calculating a remainder with pen and paper

limit :: Integral a => a
limit = 10 ^ 6
