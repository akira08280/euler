{--
  https://wiki.haskell.org/Euler_problems/51_to_60#Problem_59
--}

module WikiEuler59 (wiki_e59_solve) where

import Data.Bits
import Data.Char
import Data.List
import Data.Ord (comparing)

keys = [ [a,b,c] | a <- [97..122], b <- [97..122], c <- [97..122] ]
allAlpha = all (\k -> let a = ord k in (a >= 32 && a <= 122))
howManySpaces = length . filter (==' ')

wiki_e59_solve = do
  s <- readFile "src/resources/p059_cipher.txt"
  let
    cipher = read ("[" ++ s ++ "]") :: [Int]
    decrypts = [ map chr (zipWith xor (cycle key) cipher) | key <- keys ]
    alphaDecrypts = filter allAlpha decrypts
    message = maximumBy (comparing howManySpaces) alphaDecrypts
    asciisum = sum (map ord message)
  return asciisum
