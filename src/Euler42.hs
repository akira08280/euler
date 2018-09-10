{--
  https://projecteuler.net/problem=42

  trianglar number
  https://en.wikipedia.org/wiki/Triangular_number
--}

module Euler42 (e42_solve) where

import Data.Char (ord)
import Data.List (sort)
import System.IO (readFile)

e42_solve :: IO Int
e42_solve = do
  file <- readFile "src/resources/p042_words.txt"
  let
    names = read ("[" ++ file ++ "]") :: [String]
  return . length . filter isTriangle . map calc $ names

calc :: String -> Int
calc = sum . map (subtract 64 . ord)

isTriangle :: Integral a => a -> Bool
isTriangle x = n == n'
  where
    n = (sqrt (1 + 8 * fromIntegral x) - 1) / 2
    n' = fromIntegral . floor $ n
