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

connectsToSouth :: WireType -> Bool
connectsToSouth Vertical = True
connectsToSouth SE = True
connectsToSouth SW = True
connectsToSouth Overlap = True
connectsToSouth _ = False

connectsToEast :: WireType -> Bool
connectsToEast Horizontal = True
connectsToEast NE = True
connectsToEast SE = True
connectsToEast Overlap = True
connectsToEast _ = False

connectsToWest :: WireType -> Bool
connectsToWest Horizontal = True
connectsToWest NW = True
connectsToWest SW = True
connectsToWest Overlap = True
connectsToWest _ = False

wireToChar :: WireType -> Char
wireToChar Vertical = '|'
wireToChar Horizontal = '-'
wireToChar NE = '⌞'
wireToChar SE = '⌜'
wireToChar SW = '⌝'
wireToChar NW = '⌟'
wireToChar Overlap = '+'

allNextPoints :: WireType -> [Point]
allNextPoints w =
  catMaybes
    [ if connectsToNorth w then Just $ Point 0 1 else Nothing,
      if connectsToEast w then Just $ Point 1 0 else Nothing,
      if connectsToSouth w then Just $ Point 0 (-1) else Nothing,
      if connectsToWest w then Just $ Point (-1) 0 else Nothing
    ]

-- | Gives the next point as specified by the wire direction. The first Point
-- argument is the previous point, and the second Point argument is the
-- current point
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

data Ordinal = North | South | East | West

ordinal :: Point -> Ordinal
ordinal (Point 0 1) = North
ordinal (Point 0 (-1)) = South
ordinal (Point 1 0) = East
ordinal (Point 0 1) = West
ordinal _ = error "impossible. programmer error"

-- | Returns a wire that lets current flow through the sequence of `Points`
wireFromTo :: Point -> Point -> Point -> WireType
wireFromTo x y z = aux (ordinal $ x ->> y) (ordinal $ z ->> y)
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