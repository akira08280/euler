{--
  https://projecteuler.net/problem=89
--}

module Euler89 (e89_solve) where

import System.IO (readFile)
import Text.Regex (subRegex, mkRegex)

e89_solve :: IO Int
e89_solve = do
  file <- readFile "src/resources/p089_roman.txt"
  let
    before = subRegex (mkRegex "\\n") file ""
    after  = subRegex (mkRegex "DCCCC|LXXXX|VIIII|CCCC|XXXX|IIII") before "xx"
  return $ (length before) - (length after)
