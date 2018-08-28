{--
  https://projecteuler.net/problem=55
--}

module Euler55 (e55_solve) where

import Common (fromIntegerToInt, isPalindromic)

e55_solve :: Int
e55_solve = length . filter isLychrel $ [1..10 ^ 4]

isLychrel :: Integer -> Bool
isLychrel = all (not . isPalindromic) . map fromIntegerToInt . tail . take 51 . iterate sumToReverse

sumToReverse :: Integer -> Integer
sumToReverse n = n + (read . reverse $ show n)
