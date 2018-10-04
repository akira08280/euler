{--
  https://projecteuler.net/problem=144
--}

module Euler144 (e144_solve) where

e144_solve :: Int
e144_solve = length . takeWhile exit . iterate next $ (0.0, 10.1, 1.4, -9.6)

next :: (Floating t, Ord t) => (t, t, t, t) -> (t, t, t, t)
next (fromX, fromY, toX, toY) = (toX, toY, nextX, nextY)
  where
    -- the angle of incidence slope
    currentSlope = (fromY - toY) / (fromX - toX)
    -- Slope of ellipse
    ellipseSlope = -4 * toX / toY
    -- Calculate slope of reflection from incident slope using additive theorem of tangent
    tan = (currentSlope - ellipseSlope) / (1 + currentSlope * ellipseSlope)
    nextSlope = (ellipseSlope - tan) / (1 + tan * ellipseSlope)
    nextIntercept = toY - nextSlope * toX
    -- ax^2         + bx   + c        = 0
    -- (c^2 + 4)x^2 + 2cdx + d^2 -100 = 0
    a = nextSlope ** 2 + 4
    b = 2 * nextSlope * nextIntercept
    c = nextIntercept ** 2 - 100
    -- Formula for solving quadratic equations
    ans1 = (-b + sqrt (b ** 2 - 4 * a * c)) / (2 * a)
    ans2 = (-b - sqrt (b ** 2 - 4 * a * c)) / (2 * a)
    -- Since there are quadratic equations, there are two solutions. (Intersection)
    -- Since toX is the first intersection point, a point far from toX is taken as a solution.
    nextX = if abs (ans1 - toX) > abs (ans2 - toX) then ans1 else ans2
    nextY = nextSlope * nextX + nextIntercept

exit :: (Floating t, Ord t) => (t, t, t, t) -> Bool
exit (fromX, fromY, toX, toY) = toX > 0.01 || toX < -0.01 || toY < 0
