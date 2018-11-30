{--
  https://projecteuler.net/problem=8
--}

module Euler8 (e8Solve) where

import System.IO (readFile)
import Data.Char (digitToInt)
import Data.Text (strip, pack, unpack)

e8Solve :: IO Int
e8Solve = do
  contents <- readFile "src/resources/Euler8.txt"
  return . maximum . map product . groupBy 13 . map digitToInt . strip' $ contents

strip' :: String -> String
strip' = unpack . strip . pack

groupBy :: Int -> [a] -> [[a]]
groupBy _ [] = []
groupBy n xs = take n xs : groupBy n (tail xs)
