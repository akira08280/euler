{--
  https://projecteuler.net/problem=30
--}

module Euler30 (e30Solve) where

import Data.Char (digitToInt)

e30Solve :: Int
e30Solve =
  let
    s = sum . map ((^ 5) . digitToInt) . show
    limit = 9 ^ 5 * 6
  in
    sum . filter (\e -> e == s e) $ [2..limit]
