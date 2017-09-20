{--
  Common module for Project Euler
--}

module Common where

import Data.Bits (setBit)
import Data.Char (digitToInt)
import Data.List (delete)

stringToInt :: String -> Int
stringToInt s = read s :: Int

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

c :: Integral a => a -> a -> a
c n k = f n k `div` f k k
  where
    f n k = product [n - k + 1..n]
