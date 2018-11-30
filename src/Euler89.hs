{--
  https://projecteuler.net/problem=89
--}

module Euler89 (e89Solve) where

import System.IO (readFile)
import Text.Regex (subRegex, mkRegex)

e89Solve :: IO Int
e89Solve = do
  file <- readFile "src/resources/p089_roman.txt"
  let
    before = subRegex (mkRegex "\\n") file ""
    after  = subRegex (mkRegex "DCCCC|LXXXX|VIIII|CCCC|XXXX|IIII") before "xx"
  return $ length before - length after
