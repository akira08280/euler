{--
  https://projecteuler.net/problem=114
--}

module Euler114 (e114_solve) where

e114_solve :: Integer
e114_solve = last . ways 3 $ 50

ways :: (Num a, Enum a) => Int -> Int -> [a]
ways n i
  | i < n = replicate n 1
  | otherwise = prev ++ [append]
  where
    prev = ways n (pred i)
    append = (last prev) + (succ . sum . take ((length prev) - n) $ prev)
