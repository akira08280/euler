{--
  https://projecteuler.net/problem=105
--}

module Euler105 (e105Solve) where

import SpecialSubset (isSpecialSubset)

e105Solve :: IO Int
e105Solve = do
  file <- readFile "src/resources/p105_sets.txt"
  return . sum . map sum . filter isSpecialSubset . map (\line -> read ("[" ++ line ++ "]") :: [Int]) . lines $ file
