{--
  https://projecteuler.net/problem=8
--}

module Euler8 (e8_solve) where

import System.IO (readFile)
import Data.Char (digitToInt)
import Data.Text (strip, pack, unpack)

e8_solve :: IO Int
e8_solve = do
  contents <- readFile "src/resources/Euler8.txt"
  return . maximum . map product . groupBy' 13 . map digitToInt . strip' $ contents

groupBy' :: Int -> [a] -> [[a]]
groupBy' _ [] = []
groupBy' n xs = (take n xs) : groupBy' n (tail xs)

strip' :: String -> String
strip' = unpack . strip . pack
