{--
  https://projecteuler.net/problem=98
--}

module Euler98 (e98_solve) where

import Control.Monad (guard)
import Data.Digits (unDigits)
import Data.Function (on)
import Data.List (intercalate, nub, sort, sortBy, groupBy, permutations)
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import Data.Ord (comparing)
import System.IO (readFile)
import Common (isSquare)

e98_solve :: IO Int
e98_solve = do
  file <- readFile "src/resources/p098_words.txt"
  let
    words = read ("[" ++ file ++ "]") :: [[Char]]
    pair = map (\e -> (fst (e !! 0), fst (e !! 1))) .
           filter ((> 1) . length) .
           groupBy ((==) `on` snd) .
           sortBy (comparing snd) .
           map (\e -> (e, sort e)) .
           filter ((> 4) . length) $ words
  return . maximum . intercalate [] . map anagrams $ pair

anagrams :: ([Char], [Char]) -> [Int]
anagrams pair = do
  let
    a = fst pair
    b = snd pair
    m = Map.fromList $ zip (nub a) [0..]
    s = Map.size m
  c <- choose s [1..9]
  let
    a' = unDigits 10 . map (\e -> c !! (fromJust . Map.lookup e $ m)) $ a
    b' = unDigits 10 . map (\e -> c !! (fromJust . Map.lookup e $ m)) $ b
  guard (isSquare a' && isSquare b')
  return (max a' b')

choose :: Int -> [Int] -> [[Int]]
choose n list = concatMap permutations $ choose' list []
  where
    choose' [] r = if length r == n then [r] else []
    choose' (x:xs) r
      | length r == n = [r]
      | otherwise     = choose' xs (x:r) ++ choose' xs r
