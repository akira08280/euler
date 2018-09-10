{--
  SpecialSubset module for euler 103, 105, 106
--}

module SpecialSubset where

import Data.List (sort)
import Data.List.Unique (allUnique)
import Common (slice, subs, c, catalanNumber)

isSpecialSubset :: Integral a => [a] -> Bool
isSpecialSubset xs = allUnique xs && isUniqueSubsSum xs && isLargerSubsetsHaveLargerSum 1 (sort xs)

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

countTestCase :: Integral a => a -> a
countTestCase n = sum . map count $ [2..lim]
  where
    lim = n `div` 2
    count i = (c n i * c (n - i) i `div` 2) - (c n (2 * i) * catalanNumber i)
