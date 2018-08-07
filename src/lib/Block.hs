{--
  SpecialSubset module for euler 114, 115, 116
--}

module Block where

flexBlockWays :: (Num a, Enum a) => Int -> Int -> [a]
flexBlockWays n i
  | i < n = replicate n 1
  | otherwise = prev ++ [append]
  where
    prev = flexBlockWays n (pred i)
    append = (last prev) + (succ . sum . take ((length prev) - n) $ prev)

fixBlockWays :: Num a => Int -> Int -> [a]
fixBlockWays n i
  | i < n = replicate n 1
  | otherwise = prev ++ [append]
  where
    prev = fixBlockWays n (pred i)
    append = (last prev) + (prev !! ((length prev) - n))
