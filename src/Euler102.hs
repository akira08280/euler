{--
  https://projecteuler.net/problem=102

  If each straight line of the triangle and the origin are in the same direction, the origin is inside the triangle.

  AB*AO
  BC*BO
  CA*CO

  It is only necessary that the z-coordinates of these three outer products are equal.
--}

module Euler102 (e102Solve) where

e102Solve :: IO Int
e102Solve = do
  file <- readFile "src/resources/p102_triangles.txt"
  let
    triangles = map (\line -> read ("[" ++ line ++ "]") :: [Int]) . lines $ file
  return . length . filter id . map includeOrigin $ triangles

includeOrigin :: [Int] -> Bool
includeOrigin ns = all (< 0) [v1, v2, v3] || all (> 0) [v1, v2, v3]
  where
    v1 = (head ns * ns !! 3) - (ns !! 1 * ns !! 2)
    v2 = (ns !! 2 * ns !! 5) - (ns !! 3 * ns !! 4)
    v3 = (ns !! 4 * ns !! 1) - (ns !! 5 * head ns)
