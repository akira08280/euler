{--
  https://projecteuler.net/problem=4
--}

module Euler4 (e4_solve) where

import Common (isPalindromic)

e4_solve :: Int
e4_solve = maximum . filter isPalindromic $ (*) <$> [100..999] <*> [100..999]
