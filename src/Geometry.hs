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

-- data Orientation = North | East | South | West -- 0 deg, 90 deg, 180 deg, 270 deg
--   deriving (Eq, Show, Enum)

-- data Rotation = CW | CCW -- Clockwise/Counter-clockwise

-- -- | Rotates the first argument by the second argument
-- -- | Note that 2D rotations commute
-- rotateOrientation :: Rotation -> Orientation -> Orientation
-- rotateOrientation CW = succ
-- rotateOrientation CCW = pred

-- rotateNTimes :: Int -> Rotation -> Orientation -> Orientation
-- rotateNTimes 0 _ o = o
-- rotateNTimes n rot o = rotateNTimes (n-1) rot $ rotateOrientation rot o

-- -- | Rotate a point relative to the (local) origin
-- rotatePoint :: Rotation -> Point -> Point
-- rotatePoint CW (Point x y) = Point y (negate x)
-- rotatePoint CCW (Point x y) = Point (negate y) x
