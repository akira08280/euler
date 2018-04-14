{--
  https://projecteuler.net/problem=103
--}

module Euler103 (e103_solve) where

import Data.List (sort)
import Data.List.Unique (allUnique)
import Common (concatIntArray, slice)

e103_solve :: Int
e103_solve = concatIntArray . head . candidate $ [20, 31, 38, 39, 40, 44, 46]

candidate :: Integral a => [a] -> [[a]]
candidate = filter prune . map sort . sequence . map (\x -> [x-2..x+2])

prune :: Integral a => [a] -> Bool
prune xs = allUnique xs && isUniqueSubsSum xs && isLargerSubsetsHaveLargerSum 1 xs

isUniqueSubsSum :: Integral a => [a] -> Bool
isUniqueSubsSum xs = allUnique . map sum $ subs xs

isLargerSubsetsHaveLargerSum :: Integral a => Int -> [a] -> Bool
isLargerSubsetsHaveLargerSum i xs
  | mid < i = True
  | sum f <= sum s = False
  | otherwise = isLargerSubsetsHaveLargerSum (succ i) xs
  where
    size = length xs
    mid = size `div` 2
    f = slice xs 1 (succ i)
    s = slice xs (succ (size - i)) size

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
  where
    yss = subs xs
