{--
  https://projecteuler.net/problem=24
--}

module Euler24 (e24_solve) where

import Data.List (delete)

e24_solve :: Int
e24_solve = fromDigits $ permute [0..9] !! (pred $ 10 ^ 6)

permute :: [Int] -> [[Int]]
permute [] = [[]]
permute xs = concatMap (\x -> map (x:) $ permute $ delete x xs) xs

fromDigits :: Integral a => [a] -> a
fromDigits = foldl addDigit 0
  where
    addDigit num d = 10 * num + d
