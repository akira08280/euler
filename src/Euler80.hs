{--
  https://projecteuler.net/problem=80
  http://www.afjarvis.staff.shef.ac.uk/maths/jarvisspec02.pdf
--}

module Euler80 (e80_solve) where

import Data.Char (digitToInt)

e80_solve :: Int
e80_solve = sum . map (sum . map digitToInt . show . flip sqrtlong 100) . filter (not . isSquare) . enumFromTo 1 $ 100

sqrtlong :: Integral a => a -> a -> a
sqrtlong n d = flip div 100 . snd . head . dropWhile ((< 10 ^ (succ d)) . snd) . iterate sqrtlong' $ (5 * n, 5)
  where
    sqrtlong' (a, b)
      | a >= b = (,) ((+a) . negate $ b) ((+ 10) b)
      | otherwise = (,) ((* 100) a) ((+ 5) . (* 100) . div b $ 10)

isSquare :: Integral a => a -> Bool
isSquare x = (== x) . (^ 2) . truncate . sqrt . fromIntegral $ x
