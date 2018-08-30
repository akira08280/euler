{--
  Common module for Project Euler
--}

module Common where

import Data.Bits (setBit)
import Data.Char (digitToInt)
import Data.List (delete, tails, nub)
import Data.Numbers.Primes (primeFactors)

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

-- https://wiki.haskell.org/99_questions/Solutions/26
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = return []
combinations n xs = do
  y:xs' <- tails xs
  ys <- combinations (n-1) xs'
  return (y:ys)

c :: Integral a => a -> a -> a
c n k = f n k `div` f k k
  where
    f n k = product [n - k + 1..n]

catalanNumber :: Integral a => a -> a
catalanNumber n = c (2 * n) n `div` (n + 1)

slice :: [a] -> Int -> Int -> [a]
slice xs i k = drop (i - 1) $ take k xs

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
  where
    yss = subs xs

pas :: [[Integer]]
pas = [1] : do
  t <- pas
  let
    next = zipWith (+) (0:t) (t ++ [0])
  return next

fromIntegerToInt :: Integer -> Int
fromIntegerToInt = fromIntegral

isPalindromic :: Int -> Bool
isPalindromic n = n' == (reverse n')
  where
    n' = show n

rads :: (Num a, Enum a) => Integer -> [(a, Integer)]
rads limit = zip [1..] $ map rad [1..limit]

rad :: Integer -> Integer
rad = product . nub . primeFactors
