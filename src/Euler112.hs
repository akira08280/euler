{--
  https://projecteuler.net/problem=112
--}

module Euler112 (e112Solve) where

import Common (concatIntArray)
import Data.Char (digitToInt)
import Data.Ord (compare)
import Data.List (sortBy)

e112Solve :: Int
e112Solve = solve 1 0

solve :: Int -> Int -> Int
solve a b
  | 100 * b' == 99 * a = a
  | isBounce' = solve (succ a) b'
  | otherwise = solve (succ a) b
  where
    isBounce' = isBounce a
    b' = if isBounce' then succ b else b

isBounce :: Int -> Bool
isBounce n = n /= increasingNumber && n /= decreasingNumber
  where
    increasingNumber = sort compare n
    decreasingNumber = sort (flip compare) n

sort :: Show a => (Int -> Int -> Ordering) -> a -> Int
sort fn = concatIntArray . sortBy fn . map digitToInt . show
