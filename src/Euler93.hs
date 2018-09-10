{--
  https://projecteuler.net/problem=93

  Countdown example from chapter 9 of Programming in Haskell,
  Graham Hutton, Cambridge University Press, 2016.
--}

module Euler93 (e93_solve) where

import Data.Digits (unDigits)
import Data.List (nub, sort, minimumBy)
import Data.Ord (comparing)

data Op = Add | Sub | Mul | Div

instance Show Op where
   show Add = "+"
   show Sub = "-"
   show Mul = "*"
   show Div = "/"

apply :: Op -> Float -> Float -> Float
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x / y

interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms = foldr (concatMap . interleave) [[]]

split :: [a] -> [([a],[a])]
split []     = []
split [_]    = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

valid :: Op -> Float -> Float -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 1

valid' :: Float -> Bool
valid' n = n > 0 && truncate n == ceiling n

results :: [Float] -> [Float]
results []  = []
results [n] = [n]
results ns  = [apply o lx ly | (ls,rs) <- split ns,
                               lx      <- results ls,
                               ly      <- results rs,
                               o       <- ops,
                               valid o lx ly]

convert :: [Float] -> [Int]
convert = map truncate . sort . nub . filter valid' . concatMap results . perms

countSeq :: [Float] -> Int
countSeq ns = length . filter (uncurry (==)) $ zip [1..] seq
  where
    seq = convert ns

solve :: [([Int], Int)]
solve = do
  a <- [1..9]
  b <- [(a + 1)..9]
  c <- [(b + 1)..9]
  d <- [(c + 1)..9]
  let
    seq = countSeq [a,b,c,d]
    ans = map truncate [a,b,c,d]
  return (ans, seq)

e93_solve :: Int
e93_solve = unDigits 10 . fst . minimumBy (flip (comparing snd)) $ solve
