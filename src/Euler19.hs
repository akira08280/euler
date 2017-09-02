{--
  https://projecteuler.net/problem=19
--}

module Euler19 (e19_solve) where

import Data.Time.Calendar (fromGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)

e19_solve :: Int
e19_solve = length . filter ((==7) . third) . map toWeekDate $ fromGregorian <$> [1901..2000] <*> [1..12] <*> [1]

third :: (a, b, c) -> c
third (_, _, c) = c
