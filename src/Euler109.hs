{--
  https://projecteuler.net/problem=109
--}

module Euler109 (e109Solve) where

e109Solve :: Int
e109Solve = length $ pattern1 ++ pattern2 ++ pattern3

singles :: [Int]
singles = [1..20] ++ [25]

doubles :: [Int]
doubles = [2,4..40] ++ [50]

triples :: [Int]
triples = [3,6..60]

pattern1 :: [Int]
pattern1 = doubles

pattern2 :: [Int]
pattern2 = [score | first <- singles ++ doubles ++ triples,
                    second <- doubles,
                    let score = first + second,
                    score < 100]

pattern3 :: [Int]
pattern3 = [score | let
                      possibles = singles ++ doubles ++ triples
                      len = pred . length $ possibles,
                    i <- [0..len],
                    j <- [i..len],
                    let
                      first = possibles !! i
                      second = possibles !! j,
                    third <- doubles,
                    let score = first + second + third,
                    score < 100]
