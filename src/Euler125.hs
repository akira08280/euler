{--
  https://projecteuler.net/problem=125
--}

module Euler125 (e125Solve) where

import Common (isPalindromic)
import Control.Monad (guard)
import Data.List (nub)

e125Solve :: Int
e125Solve = sum . nub $ palindromics

palindromics :: [Int]
palindromics = do
  let
    limit' = round $ sqrt (fromIntegral limit)
  a <- [1..limit']
  consectiveSquareSums a

consectiveSquareSums :: Int -> [Int]
consectiveSquareSums n = consectiveSquareSums' n 0 []
  where
    consectiveSquareSums' n' acc xs
      | acc' > limit = xs
      | acc > 0 && isPalindromic acc' = consectiveSquareSums' (succ n') acc' (acc':xs)
      | otherwise = consectiveSquareSums' (succ n') acc' xs
      where
        acc' = n' ^ 2 + acc

limit :: Int
limit = 10 ^ 8
