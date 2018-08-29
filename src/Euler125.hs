{--
  https://projecteuler.net/problem=125
--}

module Euler125 (e125_solve) where

import Common (isPalindromic)
import Control.Monad (guard)
import Data.List (nub)

e125_solve :: Int
e125_solve = sum . nub $ palindromics

palindromics :: [Int]
palindromics = do
  let
    limit' = round((fromIntegral limit) ** 0.5)
  a <- [1..limit']
  b <- consectiveSquareSums a
  return b

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
