{--
  https://projecteuler.net/problem=49
--}

module Euler49 (e49Solve) where

import Control.Monad (guard)
import Data.List (sort)
import Data.Numbers.Primes (isPrime)
import Common (concatIntArray)

e49Solve :: Int
e49Solve = head . map concatIntArray $ terms

terms :: [[Int]] 
terms = do
  (a, b, c) <- map (\a -> (a, a + 3330, a + 6660)) [1489,1491..]
  guard (isPrime a)
  guard (isPrime b)
  guard (isPrime c)
  guard (sort (show a) == sort (show b))
  guard (sort (show b) == sort (show c))
  return [a, b, c]
