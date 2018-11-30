{--
  https://projecteuler.net/problem=19
--}

module Euler19 (e19Solve) where

import Data.Time.Calendar (fromGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Common (third)

e19Solve :: Int
e19Solve = length . filter ((==7) . third) . map toWeekDate $ fromGregorian <$> [1901..2000] <*> [1..12] <*> [1]
