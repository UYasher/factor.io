module Geometry where

import qualified Data.Map as Map

data Point = Point {pointX :: Int, pointY :: Int}
  deriving (Eq, Show, Ord)

type Grid a = Map.Map Point a

-- | Point addition
(+>>) :: Point -> Point -> Point
(Point x1 y1) +>> (Point x2 y2) = Point (x1 + x2) (y1 + y2)

-- | Point subtraction
(->>) :: Point -> Point -> Point
(Point x1 y1) ->> (Point x2 y2) = Point (x1 - x2) (y1 - y2)

adjacentTo :: Point -> Point -> Bool
(Point x1 y1) `adjacentTo` (Point x2 y2) = abs (x1 - x2) + abs (y1 - y2) == 1

negate :: Point -> Point
negate (Point x y) = Point (- x) (- y)

zero :: Point
zero = Point 0 0