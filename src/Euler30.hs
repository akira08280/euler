{--
  https://projecteuler.net/problem=30
--}

module Euler30 (e30_solve) where

import Data.Char (digitToInt)

e30_solve :: Int
e30_solve =
  let
    s = sum . map (^ 5) . map digitToInt . show
    limit = 9 ^ 5 * 6
  in
    sum . filter (\e -> e == s e) $ [2..limit]
