{--
  https://projecteuler.net/problem=111
--}

module Euler111 (e111_solve) where

import Common (combinations, digit, stringToInt)
import Control.Monad (guard, replicateM)
import MillerRabin (millerRabinPrimality)

e111_solve :: Int
e111_solve = sum . map s $ initials

d :: Int
d = 10

initials :: [[Char]]
initials = map (take d . repeat) "0123456789"

s :: [Char] -> Int
s initial = head . dropWhile (== 0) . map (sum . replaceAndPrimeFilter initial) $ [1..9]

replaceAndPrimeFilter :: [Char] -> Int -> [Int]
replaceAndPrimeFilter origin n = do
  idx <- combinations n [0..9]
  rep <- replicateM n "0123456789"
  let
    replaced = stringToInt . multiReplaceNth idx rep $ origin
  guard (length origin == digit replaced)
  guard (isPrime . toInteger $ replaced)
  return replaced

multiReplaceNth :: (Eq t, Integral a) => [a] -> [t] -> [t] -> [t]
multiReplaceNth (n:ns) (newVal:remainVals) xs
  | ns == [] = replaced
  | remainVals == [] = replaced
  | otherwise = multiReplaceNth ns remainVals replaced
  where
    replaced = replaceNth n newVal xs

replaceNth :: Integral a => a -> t -> [t] -> [t]
replaceNth n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth (n - 1) newVal xs

isPrime :: Integer -> Bool
isPrime x
  | x == 3 = True
  | otherwise = and [millerRabinPrimality x n | n <- [2,3]]
