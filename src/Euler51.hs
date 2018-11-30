{--
  https://projecteuler.net/problem=51

  Problem:
    Find the smallest prime which, by replacing part of the number (not necessarily adjacent digits) with the same digit,
    is part of an eight prime value family.

  How many digits should we replace?
  It's three.
  
  If sum of digits is divisible by 3, it is divisible by 3. So, that's not prime.
  [0,3,6,9] is congruent to 0 mod 3. As well [1,4,7] is congruent to 1 mod 3, [2,5,8] is congruent to 2 mod 3.
  When replaced 1 digit with [1..9], it comes in number divisible by 3 within numbers replaced eight times.
  As well, in case replacing 2 digits too.
  In case it's replaced 3 digits, replaced number is divisible by 3, 
  so If sum of digits not replaced is not divisible by 3, it doesn't come in number divisible by 3.
--}

module Euler51 (e51Solve) where

import Control.Monad (guard)
import Data.Char (intToDigit)
import Data.String.Utils (replace)
import Data.Numbers.Primes (isPrime, primes)
import Common (digit)

e51Solve :: Int
e51Solve = head family

family :: [Int]
family = do
  p <- filter ((> 3) . digit) primes
  let
    a = countBy 0 p == 3 && length (findPrimeFamily 0 p) == 8
    b = countBy 1 p == 3 && length (findPrimeFamily 1 p) == 8 && snd (divMod p 10) /= 1
    c = countBy 2 p == 3 && length (findPrimeFamily 2 p) == 8
  guard (a || b || c)
  return p

findPrimeFamily :: Int -> Int -> [Int]
findPrimeFamily r p = filter (\e -> isPrime e && isSameDigit e p) candidates
  where
    candidates = map (\d -> read (replace (show r) (show d) (show p)) :: Int) [0..9]

countBy :: Int -> Int -> Int 
countBy c n = length . filter (\d -> d == intToDigit c) $ show n

isSameDigit :: Int -> Int -> Bool
isSameDigit a b = digit a == digit b
