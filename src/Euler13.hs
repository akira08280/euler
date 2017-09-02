{--
  https://projecteuler.net/problem=13
--}

module Euler13 (e13_solve) where

import System.IO (readFile)

e13_solve :: IO Int
e13_solve = do
  contents <- readFile "src/resources/Euler13.txt"
  return . head' 10 $ contents

head' :: Int -> String -> Int
head' n c = read . take n . show . sum . map read $ lines c
