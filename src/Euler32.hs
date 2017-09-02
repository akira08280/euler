{--
  https://projecteuler.net/problem=32

  9 digits pandigital number combination is 1 digit and 4 digits or 2 digits or 3 digits.
  
  1d * 4d = 4d (1 + 4 + 4 = 9)
  2d * 3d = 4d (2 + 3 + 4 = 9)
--}

module Euler32 (e32_solve) where

import Data.List (nub)
import Data.Bits (setBit)
import Data.Char (digitToInt)

e32_solve :: Int
e32_solve =
  let
    a = [a * b | a <- [1..9], b <- [1234..9876], let c = concat' [a,b,a*b], isPandigital c [1..9]]
    b = [a * b | a <- [12..98], b <- [123..987], let c = concat' [a,b,a*b], isPandigital c [1..9]]
  in
    sum . nub $ a ++ b

concat' :: [Int] -> Int
concat' ns = read . concat $ map show ns

isPandigital :: Int -> [Int] -> Bool
isPandigital n a
  | digit n /= length a = False
  | otherwise = mask a == mask n'
  where
    n' = map digitToInt $ show n
    mask :: [Int] -> Int
    mask = foldr (\x y -> setBit y x) 0

digit :: Int -> Int
digit = length . show
