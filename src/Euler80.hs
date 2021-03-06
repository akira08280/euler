{--
  https://projecteuler.net/problem=80
  http://www.afjarvis.staff.shef.ac.uk/maths/jarvisspec02.pdf
--}

module Euler80 (e80Solve) where

import Data.Char (digitToInt)
import Common (isSquare)

e80Solve :: Int
e80Solve = sum . map (sum . map digitToInt . show . flip sqrtlong 100) . filter (not . isSquare) . enumFromTo 1 $ 100

sqrtlong :: Integral a => a -> a -> a
sqrtlong n d = flip div 100 . snd . head . dropWhile ((< 10 ^ succ d) . snd) . iterate sqrtlong' $ (5 * n, 5)
  where
    sqrtlong' (a, b)
      | a >= b = (,) ((+a) . negate $ b) ((+ 10) b)
      | otherwise = (,) ((* 100) a) ((+ 5) . (* 100) . div b $ 10)
