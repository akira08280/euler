{--
  https://projecteuler.net/problem=102

  If each straight line of the triangle and the origin are in the same direction, the origin is inside the triangle.

  AB*AO
  BC*BO
  CA*CO

  It is only necessary that the z-coordinates of these three outer products are equal.
--}

module Euler102 (e102_solve) where

e102_solve :: IO Int
e102_solve = do
  file <- readFile "src/resources/p102_triangles.txt"
  let
    triangles = map (\line -> read ("[" ++ line ++ "]") :: [Int]) . lines $ file
  return . length . filter (\t -> t) . map includeOrigin $ triangles

includeOrigin :: [Int] -> Bool
includeOrigin ns = all (< 0) [v1, v2, v3] || all (> 0) [v1, v2, v3]
  where
    v1 = (ns !! 0 * ns !! 3) - (ns !! 1 * ns !! 2)
    v2 = (ns !! 2 * ns !! 5) - (ns !! 3 * ns !! 4)
    v3 = (ns !! 4 * ns !! 1) - (ns !! 5 * ns !! 0)
