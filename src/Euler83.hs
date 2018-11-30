{--
  https://projecteuler.net/problem=83
--}

module Euler83 (e83Solve) where

import Control.Arrow (first, second)
import Data.Array ((!), bounds, elems, inRange, listArray, range, Array)
import Data.Maybe (fromJust)
import Data.Graph.Inductive (Gr)
import Data.Graph.Inductive.Graph (UNode, LEdge, mkGraph)
import Data.Graph.Inductive.Query.SP (spTree)
import Data.Graph.Inductive.Internal.RootPath (getDistance)
import Data.List (minimumBy)
import Data.Ord (comparing)
import System.IO (readFile)

e83Solve :: IO Int
e83Solve = do
  file <- readFile "src/resources/p083_matrix.txt"
  let
    list = map (\line -> read ("[" ++ line ++ "]") :: [Int]) . lines $ file
    side = length $ head list
    mx = listArray ((0,0), (pred side, pred side)) . concat $ list
    graph = genGraph mx side
    from = [(0,0)]
    to = [(pred side, pred side)]
    spTrees = [(,) f tree | f <- from, let tree = spTree (fromIxToNode f side) graph]
    distances = [(,) (f, t) (distance + (mx ! f)) | (f, tree) <- spTrees,
                                                    t <- to,
                                                    let distance = fromJust $ getDistance (fromIxToNode t side) tree]
  return . snd . minimumBy (comparing snd) $ distances

genNodes :: Int -> [UNode]
genNodes side = zip [0..(side ^ 2 - 1)] . cycle $ [()]

genEdges :: Array (Int, Int) Int -> Int -> [LEdge Int]
genEdges mx side = [edge | i <- range . bounds $ mx,
                           shift <- shifts,
                           let i' = shift i,
                           inRange (bounds mx) i',
                           let edge = (fromIxToNode i side, fromIxToNode i' side, mx ! i')]

genGraph :: Array (Int, Int) Int -> Int -> Gr () Int
genGraph mx side = mkGraph (genNodes side) (genEdges mx side)

fromIxToNode :: (Int, Int) -> Int -> Int
fromIxToNode (row, col) side = (+ col) . (* side) $ row

shifts :: [(Int, Int) -> (Int, Int)]
shifts = [first succ, second succ, first pred, second pred]
