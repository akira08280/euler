{--
  https://projecteuler.net/problem=118
--}

module Euler118 (e118Solve) where

import Common (permute)
import MillerRabin (isPrime)

e118Solve :: Int
e118Solve = sum . map (length . divide) . filter (odd . last) . permute $ [1..9]

divide :: [Integer] -> [[Integer]]
divide = filter (not . null) . divide' []
  where
    divide' _ [] = [[]]
    divide' ps [x]
      | isLargerThanLast ps x && isPrime x = [ps ++ [x]]
      | otherwise = [[]]
    divide' ps (x:y:xs)
      | isLargerThanLast ps x && isPrime x = divide' (ps ++ [x]) (y:xs) ++ divide' ps (x*10+y:xs)
      | otherwise = divide' ps (x*10+y:xs)

isLargerThanLast :: Ord a => [a] -> a -> Bool
isLargerThanLast [] x = True
isLargerThanLast ps x
  | last ps < x = True
  | otherwise = False
