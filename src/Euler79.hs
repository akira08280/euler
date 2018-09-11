{--
  https://projecteuler.net/problem=79

  Pen and Paper solution.

  Passcode that exclude duplicated is here.
  
  129,160,162,168,180,289,290,316,318,319,362,368,380,389,620,629,680,689,690,710,716,718,719,720,728,729,731,736,760,762,769,790,890

  To find shortest possible secret passcode, it is excluded from passcode both 4 and 5. (Not in above list.)
  First digit of left is 7, because there are not the left than 7 in above list. As same reason, last digit is 0. (7******0)
  The candidates of next is 1, 2, 3, 6, 9. But 1, 6, 9 are exclueded by 731, 736, 719. Then, 2 is excluded by 362. Therefore, next is 3. (73*****0)
  We can easily find next digit by passcode beginning from 3. So next is 1.  (731****0)
  The rest are 2,6,8,9.
  If we look at the number of beginning from 6, we can understand next number is 6. (7316***0)
  After we see that number beginning from 2, then we can find where is located rest number. (73162890)

  Algorithm.

  we count up quantity the left side digits of each digit, then sort each digit in ascending order of quantity.
  For example, there are not the left of 7, so 7 is first. as same, there are 1,2,3,6,7,8,9 of the left of 0.

  [(0, [1,2,3,6,7,8,9]), (1,[3,7]), (7,[])]

  We can find result by sorting the size of second of tuple.
--}

module Euler79 (e79_solve) where

import Control.Monad (void)
import Control.Monad.State (put, get, evalState, State)
import Data.Char (digitToInt)
import Data.Digits (unDigits)
import Data.List (nub, sortBy)
import Data.Ord (comparing)
import System.IO (readFile)
import qualified Data.Map as Map (Map, empty, findWithDefault, insert, toList)
import qualified Data.Set as Set (Set, empty, insert, size)

e79_solve :: IO Int
e79_solve = do
  file <- readFile "src/resources/p079_keylog.txt"
  let
    keylog = map (map digitToInt) . lines $ file
  return . findPasscode $ keylog

-- State monad
findPasscode :: [[Int]] -> Int
findPasscode keylog = evalState (findPasscode' keylog') Map.empty
  where
    keylog' = nub . map reverse $ keylog

-- sort by State. Set size of second of tuple.
findPasscode' :: [[Int]] -> State (Map.Map Int (Set.Set Int)) Int
findPasscode' keylog = do
  mapM_ up keylog
  get >>= (\m -> return (convert $ Map.toList m))
  where
    convert = unDigits 10 . map fst . sortBy (comparing $ Set.size . snd)

-- update State set left side number of oneself
up :: [Int] -> State (Map.Map Int (Set.Set Int)) ()
up [] = return ()
up (self:lefts) = do
  if null lefts then
    insert' self . negate $ 1 -- dirty !
  else
    mapM_ (insert' self) lefts
  void (up lefts)

-- insert with left from self.
insert' :: Int -> Int -> State (Map.Map Int (Set.Set Int)) ()
insert' self left = do
  m <- get
  let
    s = Map.findWithDefault Set.empty self m
  put (Map.insert self (Set.insert left s) m)
