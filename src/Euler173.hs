{--
  https://projecteuler.net/problem=173
--}

module Euler173 (e173Solve) where

e173Solve :: Integer
e173Solve = sum . map (\x -> m `div` x - x) $ [1..upper]
  where
    upper = ceiling . sqrt . fromIntegral $ m

m :: Integral a => a
m = tiles `div` 4 

tiles :: Integral a => a
tiles = 10 ^ 6
