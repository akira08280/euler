{--
  SpecialSubset module for euler 114, 115
--}

module Block where

ways :: (Num a, Enum a) => Int -> Int -> [a]
ways n i
  | i < n = replicate n 1
  | otherwise = prev ++ [append]
  where
    prev = ways n (pred i)
    append = (last prev) + (succ . sum . take ((length prev) - n) $ prev)
