{--
  https://projecteuler.net/problem=119
--}

module Euler119 (e119Solve) where

import Data.Char (digitToInt)
import Data.List (sort)
import Control.Monad (guard)

e119Solve :: Integer
e119Solve = sort digitPowerSums !! 29

digitPowerSums :: [Integer]
digitPowerSums = do
  b <- [2..100]
  e <- [2..10]
  let
    t = b ^ e
  guard (sumOfDigit t == b)
  return t

sumOfDigit :: Show a => a -> Integer
sumOfDigit = toInteger . sum . map digitToInt . show
