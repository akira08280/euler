{--
  https://projecteuler.net/problem=107
--}

module Euler107 (e107_solve) where

import Data.Graph.Inductive (Gr)
import Data.Graph.Inductive.Graph (UNode, LEdge, mkGraph, delLEdge, labEdges)
import Data.Graph.Inductive.Query.DFS (isConnected)
import Data.List (sortBy)
import Data.List.Split (splitOn)
import System.IO (readFile)

e107_solve :: IO Int
e107_solve = do
  file <- readFile "src/resources/p107_network.txt"
  let
    list = map (map (\e -> if e == "-" then 0 else read e :: Int)) . map (splitOn ",") . lines $ file
    nodes = genNodes . length $ list
    edges = sortBy (\(_, _, a) (_, _, b)-> compare b a) . genEdges $ list
    before = genGraph nodes edges
    after = prune edges before
  return $ sumOfEdges before - sumOfEdges after

genNodes :: Int -> [UNode]
genNodes countOfNodes = zip [0..(countOfNodes - 1)] . cycle $ [()]

genEdges :: [[Int]] -> [LEdge Int]
genEdges list = [(i, j, e) | let lim = pred . length $ list,
                             i <- [0..lim],
                             j <- [i..lim],
                             let e = list !! i !! j,
                             e > 0]

genGraph :: [UNode] -> [LEdge Int] -> Gr () Int
genGraph nodes edges = mkGraph nodes edges

sumOfEdges :: Gr () Int -> Int
sumOfEdges = sum . map (\(_, _, a) -> a) . labEdges

prune :: [LEdge Int] -> Gr () Int -> Gr () Int
prune [] gr = gr
prune (x:xs) gr
  | isConnected gr' = prune xs gr'
  | otherwise = prune xs gr
  where
    gr' = delLEdge x gr
