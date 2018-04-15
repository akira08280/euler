{--
  https://projecteuler.net/problem=105
--}

module Euler105 (e105_solve) where

import SpecialSubset (isSpecialSubset)

e105_solve :: IO Int
e105_solve = do
  file <- readFile "src/resources/p105_sets.txt"
  return . sum . map sum . filter isSpecialSubset . map (\line -> read ("[" ++ line ++ "]") :: [Int]) . lines $ file
