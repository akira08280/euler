{--
  https://projecteuler.net/problem=55
--}

module Euler55 (e55_solve) where

e55_solve :: Int
e55_solve = length . filter isLychrel $ [1..10 ^ 4]

isLychrel :: Integer -> Bool
isLychrel = all isNotPalindromic . tail . take 51 . iterate sumToReverse

sumToReverse :: Integer -> Integer
sumToReverse n = n + (read . reverse $ show n)

isNotPalindromic :: Integer -> Bool
isNotPalindromic n = show n /= (reverse $ show n)
