module Wire where

import Data.Maybe
import Geometry
import ResourceUpdate
import State

-- | Gives the direction a wire runs, which determines what wires it connects to and propagates values to
data WireType = Vertical | Horizontal | NW | NE | Overlap | SW | SE
  deriving (Eq, Show, Read, Enum, Bounded, Ord)

-- | Returns True iff the given wire can propagate values to the north
connectsToNorth :: WireType -> Bool
connectsToNorth Vertical = True
connectsToNorth NE = True
connectsToNorth NW = True
connectsToNorth Overlap = True
connectsToNorth _ = False

-- | Returns True iff the given wire can propagate values to the south
connectsToSouth :: WireType -> Bool
connectsToSouth Vertical = True
connectsToSouth SE = True
connectsToSouth SW = True
connectsToSouth Overlap = True
connectsToSouth _ = False

-- | Returns True iff the given wire can propagate values to the east
connectsToEast :: WireType -> Bool
connectsToEast Horizontal = True
connectsToEast NE = True
connectsToEast SE = True
connectsToEast Overlap = True
connectsToEast _ = False

-- | Returns True iff the given wire can propagate values to the west
connectsToWest :: WireType -> Bool
connectsToWest Horizontal = True
connectsToWest NW = True
connectsToWest SW = True
connectsToWest Overlap = True
connectsToWest _ = False

-- For internal debugging use only
wireToChar :: WireType -> Char
wireToChar Vertical = '|'
wireToChar Horizontal = '-'
wireToChar NE = '⌞'
wireToChar SE = '⌜'
wireToChar SW = '⌝'
wireToChar NW = '⌟'
wireToChar Overlap = '+'

-- | Returns a list of `Point`s in local coordinate space that the given wire
-- connects to.
allNextPoints :: WireType -> [Point]
allNextPoints w =
  catMaybes
    [ if connectsToNorth w then Just $ Point 0 1 else Nothing,
      if connectsToEast w then Just $ Point 1 0 else Nothing,
      if connectsToSouth w then Just $ Point 0 (-1) else Nothing,
      if connectsToWest w then Just $ Point (-1) 0 else Nothing
    ]

-- | Gives the next point current will flow into, as specified by the wire
-- direction. The first Point argument is the previous point, and the second
-- Point argument is the current point
nextPointFrom :: WireType -> Point -> Point -> Point
nextPointFrom w previous current =
  -- overlaps can return multiple next points that aren't previous.
  -- all other wires return one non-previous point
  case filter (/= (previous ->> current)) (allNextPoints w) of
    [x] -> current +>> x
    _ -> current +>> (current ->> previous)

-- | Tells whether the current/signal "bends" when propagating through the wire
-- (ie, if it's a corner)
currentBends :: WireType -> Bool
currentBends NE = True
currentBends SE = True
currentBends SW = True
currentBends NW = True
currentBends _ = False

-- Helpful datatype for some testing and debugging
data Direction = North | South | East | West deriving (Show)

-- Converts the given argument to a `Direction`. Errors if the argument is not
-- in local coordinate space.
-- For internal debugging use only.
pointToDir :: Point -> Direction
pointToDir (Point 0 1) = North
pointToDir (Point 0 (-1)) = South
pointToDir (Point 1 0) = East
pointToDir (Point (-1) 0) = West
pointToDir _ = error "impossible. programmer error"

-- The inverse of `pointToDir`.
-- For internal debugging use only.
dirToPoint :: Direction -> Point
dirToPoint North = Point 0 1
dirToPoint East = Point 1 0
dirToPoint South = Point 0 (-1)
dirToPoint West = Point (-1) 0

-- | Returns a wire that lets current flow through the sequence of `Points`.
wireFromTo :: Point -> Point -> Point -> WireType
wireFromTo x y z = aux (pointToDir $ x ->> y) (pointToDir $ z ->> y)
  where
    aux North North = Vertical
    aux North South = Vertical
    aux North East = NE
    aux North West = NW
    aux South North = Vertical
    aux South South = Vertical
    aux South East = SE
    aux South West = SW
    aux East North = NE
    aux East South = SE
    aux East East = Horizontal
    aux East West = Horizontal
    aux West North = NW
    aux West South = SW
    aux West East = Horizontal
    aux West West = Horizontal

-- | Places the first argument onto the second argument, to fuse them if possible.
-- Returns the first argument if fusion is not possible
placeOnto :: WireType -> WireType -> WireType
placeOnto Horizontal Vertical = Overlap
placeOnto Vertical Horizontal = Overlap
placeOnto x _ = x