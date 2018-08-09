{--
  https://projecteuler.net/problem=118
--}

module Euler118 (e118_solve) where

import Common (permute)
import MillerRabin (millerRabinPrimality)

e118_solve :: Int
e118_solve = sum . map (length . divide) . filter (odd . last) . permute $ [1..9]

divide :: [Integer] -> [[Integer]]
divide = filter (not . null) . divide' []
  where
    divide' _ [] = [[]]
    divide' ps [x]
      | largerThanLast ps x && isPrime x = [ps ++ [x]]
      | otherwise = [[]]
    divide' ps (x:y:xs)
      | largerThanLast ps x && isPrime x = divide' (ps ++ [x]) (y:xs) ++ divide' ps (x*10+y:xs)
      | otherwise = divide' ps (x*10+y:xs)

largerThanLast :: Ord a => [a] -> a -> Bool
largerThanLast [] x = True
largerThanLast ps x
  | last ps < x = True
  | otherwise = False

isPrime :: Integer -> Bool
isPrime x
  | x == 1 = False
  | x == 2 = True
  | x == 3 = True
  | otherwise = and [millerRabinPrimality x n | n <- [2,3]]
