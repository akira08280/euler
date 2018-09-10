{--
  https://wiki.haskell.org/Euler_problems/11_to_20#Problem_11
--}

module WikiEuler11 (wiki_e11_solve) where

import Control.Arrow ((***), first, second)
import Data.Array

input :: String -> Array (Int,Int) Int
input = listArray ((1, 1),(20, 20)) . map read . words

senses = [first (+ 1), (+1) *** (+1), second (+ 1), (+1) *** (\n -> n - 1)]

inArray a = inRange (bounds a)

prods :: Array (Int, Int) Int -> [Int]
prods a = [product xs | i <- range $ bounds a,
                        s <- senses,
                        let is = take 4 $ iterate s i,
                        all (inArray a) is,
                        let xs = map (a!) is]

wiki_e11_solve :: IO Int
wiki_e11_solve = do
  contents <- readFile "src/resources/Euler11.txt"
  return . maximum . prods . input $ contents
