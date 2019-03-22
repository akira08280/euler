{--
  https://projecteuler.net/problem=169
--}

module Euler169 (e169Solve) where

e169Solve :: Integral a => a
e169Solve = f (10 ^ 25)

f :: Integral a => a -> a
f n = fst . f' n $ (1, 0) 
  where
    f' n (a, b)
      | n <= 0 = (a, b)
      | odd n  = f' (n `div` 2) (a + b, b)
      | even n = f' (n `div` 2) (a, a + b)
