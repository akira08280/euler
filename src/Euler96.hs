{--
  https://projecteuler.net/problem=96
--}

module Euler96 (e96_solve) where

import Common (stringToInt)
import Data.List (nub, elemIndex, (\\))
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust, isNothing)
import Text.Regex.Posix ((=~))

isSameRow :: Int -> Int -> Bool
isSameRow i j = (i `div` 9) == (j `div` 9)

isSameCol :: Int -> Int -> Bool
isSameCol i j = (i - j) `mod` 9 == 0

isSameBox :: Int -> Int -> Bool
isSameBox i j = (i `div` 27) == (j `div` 27) && (i `mod` 9 `div` 3) == (j `mod` 9 `div` 3) 

isTarget :: Int -> Int -> Bool
isTarget i j = isSameRow i j || isSameCol i j || isSameBox i j

writtenNumbers :: Int -> String -> String
writtenNumbers i grid = nub . map (grid !!) . filter (isTarget i) $ [0..80]

possibles :: Int -> String -> [String]
possibles zeroIndex grid = [take zeroIndex grid ++ [w] ++ drop (succ zeroIndex) grid | w <- "948721536" \\ writtenNumbers zeroIndex grid]

solve :: String -> [String]
solve grid
  | isNothing zeroIndex = [grid]
  | null grids          = []
  | otherwise           = concatMap solve grids
  where
    zeroIndex = elemIndex '0' grid
    grids = possibles (fromJust zeroIndex) grid

e96_solve :: IO Int
e96_solve = do
  file <- readFile "src/resources/p096_sudoku.txt"
  let
    contents = filter (=~ "^[0-9]") . lines $ file
    grids = map (foldl1 (++)) . chunksOf 9 $ contents
  return . sum . map (stringToInt . take 3 . head . solve) $ grids
