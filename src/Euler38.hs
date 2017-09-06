{--
  https://projecteuler.net/problem=38

  1. Fixed number is less than 5 digits, because multiple number is greater than 1.
     If fixed number greater than 5 digits, sum of digits is over 9 digits.
     
     e.g) 10000 * 1 = 5 digits, 10001 * 2 = 5 digits, sum of two is 10 digits.

  2. Fixed number is 4 digits. Let's assume that fixed number start with 9, because of getting largest number.
     2 or 3 digits integers start with 9 is impossible to make 9 digits.
     
     e.g) In case of fixed number is 2 digits. 9X * 1 = 2 digits, 9X * 2 = 3 digits, 9X * 3 = 3 digits, 9X * 4 = 3 digits.. cannot make 9.
          In case of fixed number is 3 digits. 9XX * 1 = 3 digits, 9XX * 2 = 4 digits, 9XX * 3 = 4 digits.. cannot make 9.  
          In case of fixed number is 4 digits. 9XXX * 1 = 4 digits, 9XXX * 2 = 5 digits, can make 9.
          In case of fixed number is 5 digits. 9XXXX * 1 = 5 digits, 9XXX * 2 = 6 digits, can make 9.
  
  3. Second digit from left side of 4 digits less equal than 4.
     If greater than equal 5, result consists of 19XXX like two of 9.
--}

module Euler38 (e38_solve) where

import Data.Bits (setBit)
import Data.Char (digitToInt)
import Common (digit, isPandigital, concatIntArray)

e38_solve :: Int
e38_solve =
  let
    a = head . filter (\e -> isPandigital (concatIntArray [e, e * 2]) [1..9]) $ [9387,9386..]
  in
    concatIntArray [a, a * 2]
