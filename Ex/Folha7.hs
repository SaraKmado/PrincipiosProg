--1
data Point = Point (Float,Float) -- (x,y)
data Shape = Circle Point Float | Rectangle Point Point | Triangle Point Point Point
-- Cirlce as Center(x,y) radius, Rectangle as (x1,y1) (x2,y2), Triangle as its 3 points

perimeter :: Shape -> Float
perimeter (Circle _ radius) = 2 * radius * pi
perimeter (Rectangle (Point (x1,y1)) (Point (x2,y2))) = 2 * abs(x2-x1) + 2 * abs(y2-y1)
perimeter (Triangle (Point (x1,y1)) (Point (x2,y2)) (Point (x3,y3))) = dist (x1,y1) (x2,y2) + dist (x2,y2) (x3,y3) + dist (x1,y1) (x3,y3)

dist :: (Float,Float) -> (Float,Float) -> Float
dist (x1,y1) (x2,y2) = sqrt $ (x1 - x2)^2 + (y1 - y2)^2

isRegular :: Shape -> Bool
isRegular (Circle _ _) = True
isRegular (Rectangle (Point (x1,y1)) (Point (x2,y2))) = x1 - x2 == y1 - y2
isRegular (Triangle (Point p1) (Point p2) (Point p3)) =
  dist2 (Point p1) (Point p2) == dist2 (Point p2) (Point p3) && dist2 (Point p1) (Point p3) == dist2 (Point p1) (Point p2)

dist2 :: Point -> Point -> Float
dist2 (Point (x1,y1)) (Point (x2,y2)) = sqrt $ (x1 - x2)^2 + (y1 - y2)^2

data Nat = Zero | Succ Nat
