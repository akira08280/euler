{--
  https://projecteuler.net/problem=90
--}

module Euler90 (e90Solve) where

import Control.Monad (guard)
import Common (combinations)

e90Solve:: Int
e90Solve = length solve

solve :: [([Int], [Int])]
solve = do
  dices <- combinations 2 . combinations 6 $ [0,1,2,3,4,5,6,7,8,6]
  let
    dice1 = head dices
    dice2 = dices !! 1
  guard (valid dice1 dice2)
  return (dice1, dice2)

valid :: [Int] -> [Int] -> Bool
valid d1 d2 = all (\(x, y) -> elem x d1 && elem y d2 || elem x d2 && elem y d1) squares

squares :: [(Int, Int)]
squares = [(0,1),(0,4),(0,6),(1,6),(2,5),(3,6),(4,6),(8,1)]
