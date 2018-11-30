{--
  https://projecteuler.net/problem=13
--}

module Euler13 (e13Solve) where

import System.IO (readFile)

e13Solve :: IO Int
e13Solve = do
  contents <- readFile "src/resources/Euler13.txt"
  return . head' 10 $ contents

head' :: Int -> String -> Int
head' n c = read . take n . show . sum . map read $ lines c
