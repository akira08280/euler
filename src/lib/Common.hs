{--
  Common module for Project Euler
--}

module Common where

import Data.Bits (setBit)
import Data.Char (digitToInt)
import Data.List (delete)

groupBy :: Int -> [a] -> [[a]]
groupBy _ [] = []
groupBy n xs = (take n xs) : groupBy n (tail xs)

third :: (a, b, c) -> c
third (_, _, c) = c

isSquare :: Integral a => a -> Bool
isSquare x = (== x) . (^ 2) . truncate . sqrt . fromIntegral $ x

digit :: Integral a => a -> Int
digit 0 = 1
digit n = digit' 0 n
  where
    digit' c n
      | n < 0 = error "Argument is invalid."
      | n == 0 = c
      | otherwise = digit' (succ c) (n `div` 10)

isPandigital :: Int -> [Int] -> Bool
isPandigital n a
  | digit n /= length a = False
  | otherwise = mask a == mask n'
  where
    n' = map digitToInt $ show n

mask :: [Int] -> Int
mask = foldr (\x y -> setBit y x) 0

concatIntArray :: [Int] -> Int
concatIntArray ns = read . concat $ map show ns

permute :: Eq a => [a] -> [[a]]
permute [] = [[]]
permute xs = concatMap (\x -> map (x:) $ permute $ delete x xs) xs
