{--
  https://projecteuler.net/problem=4
--}

module Euler4 (e4Solve) where

import Common (isPalindromic)

e4Solve :: Int
e4Solve = maximum . filter isPalindromic $ (*) <$> [100..999] <*> [100..999]
