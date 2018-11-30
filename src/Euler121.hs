{--
  https://projecteuler.net/problem=121

  generate function
  (x + 1)(x + 2)(x + 3)(x + 4)(x + 5)(x + 6)(x + 7)....(x + 15)
--}

module Euler121 (e121Solve) where

e121Solve :: Integer
e121Solve = sum total `div` sum wins
  where
    total = cofficients !! turns
    wins = take (turns `div` 2 + 1) total

cofficients :: [[Integer]]
cofficients = [1] : do
  t <- cofficients
  let
    red = toInteger . length $ t
    diff = map (* red) t
    next = zipWith (+) (t ++ [0]) (0:diff)
  return next

turns :: Int
turns = 15
