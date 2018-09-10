{--
  https://projecteuler.net/problem=22

  We have to subtract 64 to get score from each character, because ord 'A' is 65.
--}

module Euler22 (e22_solve) where

import Data.Char (ord)
import Data.List (sort)
import System.IO (readFile)

e22_solve :: IO Int
e22_solve = do
  file <- readFile "src/resources/p022_names.txt"
  let
    names = read ("[" ++ file ++ "]") :: [String]
  return . sum . zipWith calc [1..] . sort $ names

calc :: Int -> String -> Int
calc i = (* i) . sum . map (subtract 64 . ord)
