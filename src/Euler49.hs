{--
  https://projecteuler.net/problem=49
--}

module Euler49 (e49_solve) where

import Control.Monad (guard)
import Data.List (sort)
import Data.Numbers.Primes (isPrime)

e49_solve :: Int
e49_solve = head . map concat' $ terms

terms :: [[Int]] 
terms = do
  (a, b, c) <- map (\a -> (a, a + 3330, a + 6660)) [1489,1491..]
  guard (isPrime a)
  guard (isPrime b)
  guard (isPrime c)
  guard (sort (show a) == sort (show b))
  guard (sort (show b) == sort (show c))
  return [a, b, c]

concat' :: [Int] -> Int
concat' ns = read . concat $ map show ns
