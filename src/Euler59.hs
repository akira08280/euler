{--
  https://projecteuler.net/problem=59
 
  For example, 

  Plain text
    Using a brute force attack, can you decrypt the cipher using XOR encryption?
  Key
    abc
  
  Using a brute force attack, can you decrypt the cipher using XOR encryption? 
  xor
  abcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabca
  
  Assume that it is space most frequent letter of each group which cipher is divided by each 3 letters.

  ciphe xor key = plain
  plain xor key = cipher
  cipher xor plain = key
  the most frequent letter xor space = key corresponding each group
 
  the most frequent letter in English is space. (ASKII CODE:32)
--}

module Euler59 (e59_solve) where

import Control.Arrow ((&&&))
import Data.Bits (xor)
import Data.Char (chr, ord)
import Data.Function (on)
import Data.Ord (comparing)
import Data.List (sort, sortBy, group, groupBy, maximumBy)

e59_solve :: IO Int
e59_solve = do
  s <- readFile "src/resources/p059_cipher.txt"
  let
    cipher = (read ("[" ++ s ++ "]") :: [Int])
    key = analize cipher
  return . sum . map ord . decode key $ cipher

decode :: [Char] -> [Int] -> [Char]
decode key cipher = map (\(k, c) -> chr . xor c $ (ord k)) $ zip (cycle key) cipher

analize :: [Int] -> [Char]
analize xs = map (chr . xor 32) freq
  where
    group' = groupBy ((==) `on` fst) . sortBy (compare `on` fst) $ zip (cycle [0..2]) xs
    freq = map (fst .
                maximumBy (comparing snd) .
                map (head &&& length) .
                group .
                sort .
                map snd) $ group'
