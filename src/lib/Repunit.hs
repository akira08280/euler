{--
  Repunit module for euler 129
--}

module Repunit where

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
