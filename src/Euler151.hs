{--
  https://projecteuler.net/problem=151
--}

module Euler151 (e151Solve) where

import Common (decimalRound)

e151Solve :: Double
e151Solve = decimalRound (expected 1 1 1 1 1) 6

expected :: (Eq a, Enum a, Fractional a) => a -> a -> a -> a -> a -> a
expected v 1 0 0 0    = v + expected v 0 1 1 1
expected v 0 1 0 0    = v + expected v 0 0 1 1
expected v 0 0 1 0    = v + expected v 0 0 0 1
expected v 0 0 0 1    = 0
expected v (-1) _ _ _ = 0
expected v _ (-1) _ _ = 0
expected v _ _ (-1) _ = 0
expected v _ _ _ (-1) = 0
expected v a2 a3 a4 a5 = nextIfPickUpA2 + nextIfPickUpA3 + nextIfPickUpA4 + nextIfPickUpA5
  where
    sum = a2 + a3 + a4 + a5
    nextIfPickUpA2 = expected (v * a2 / sum) (pred a2) (succ a3) (succ a4) (succ a5)
    nextIfPickUpA3 = expected (v * a3 / sum) a2 (pred a3) (succ a4) (succ a5)
    nextIfPickUpA4 = expected (v * a4 / sum) a2 a3 (pred a4) (succ a5)
    nextIfPickUpA5 = expected (v * a5 / sum) a2 a3 a4 (pred a5)
