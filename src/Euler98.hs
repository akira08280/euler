{--
  https://projecteuler.net/problem=98
--}

module Euler98 (e98Solve) where

import Control.Monad (guard)
import Common (isSquare)
import Data.Digits (unDigits)
import Data.Function (on)
import Data.List (intercalate, nub, sort, sortBy, groupBy, permutations)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import System.IO (readFile)
import qualified Data.Map as Map (fromList, size, lookup)

e98Solve :: IO Int
e98Solve = do
  file <- readFile "src/resources/p098_words.txt"
  let
    words = read ("[" ++ file ++ "]") :: [String]
    pair = map (\e -> (fst (head e), fst (e !! 1))) .
           filter ((> 1) . length) .
           groupBy ((==) `on` snd) .
           sortBy (comparing snd) .
           map (\e -> (e, sort e)) .
           filter ((> 4) . length) $ words
  return . maximum . intercalate [] . map anagrams $ pair

anagrams :: (String, String) -> [Int]
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
    choose' [] r = [r | length r == n]
    choose' (x:xs) r
      | length r == n = [r]
      | otherwise     = choose' xs (x:r) ++ choose' xs r
