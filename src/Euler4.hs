{--
  https://projecteuler.net/problem=4
--}

module Euler4 (e4_solve) where

e4_solve :: Int
e4_solve = maximum . filter isPalindromic $ (*) <$> [100..999] <*> [100..999]

isPalindromic :: Int -> Bool
isPalindromic n = show n == (reverse $ show n)
