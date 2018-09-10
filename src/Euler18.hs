{--
  https://projecteuler.net/problem=18
--}

module Euler18 (e18_solve) where

import System.IO (readFile)
import Control.Arrow ((***))
import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Data.Array.ST (STUArray, newListArray, readArray, writeArray)
import Data.List (intercalate)

e18_solve :: IO Int
e18_solve = do
  file <- readFile "src/resources/Euler18.txt"
  let
    contents = intercalate [] $ fillArray 0 $ map (map read . words) $ lines file
    edge = round $ sqrt $ fromIntegral (length contents)
    result = dp edge contents
  return result

dp :: Int -> [Int] -> Int
dp edge contents = runST $ do
  cache <- newListArray ((0,0), (edge-1, edge-1)) contents :: ST s (STUArray s (Int, Int) Int)
  forM_ seq $ \index -> do
    current <- readArray cache index
    leftBottom <- readArray cache (succ (fst index), snd index)
    rightBottom <- readArray cache ((succ *** succ) index)
    writeArray cache index $ maximum $ current + leftBottom : [current + rightBottom]
  readArray cache (0, 0)
  where
    seq = (,) <$> [edge-2, edge-3..0] <*> [0..edge-2]

fillArray :: Integral a => a -> [[a]] -> [[a]]
fillArray n array = map (\e -> e ++ replicate (len - length e) n) array
  where
    len = maximum $ map length array
