module Blueprint where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Geometry
import Machine
import ResourceUpdate
import State

data Blueprint = Blueprint
  { fixedPoints :: Set.Set Point,
    grid :: Grid Machine,
    minimumSinksToSatisfy :: Int,
    width :: Int,
    height :: Int
  }
  deriving (Eq, Show)

blankBlueprint :: Int -> Int -> Blueprint
blankBlueprint = Blueprint Set.empty Map.empty 0

-- Operations to Edit Factories

-- | Returns `True` iff the given point is within the bounds of the blueprint
isInBounds :: Point -> Blueprint -> Bool
isInBounds (Point x y) Blueprint {width = width, height = height} =
  0 <= x && x < width && 0 <= y && y < height

-- | Returns `True` iff the point is in-bounds and not a fixed (ie non-user-editable) point
isEditable :: Point -> Blueprint -> Bool
isEditable p b@Blueprint {fixedPoints = fixedPoints} =
  isInBounds p b && p `notElem` fixedPoints

-- | Returns `True` iff a one-grid machine could be placed at the point
isAvailable :: Point -> Blueprint -> Bool
isAvailable p b@Blueprint {grid = grid} =
  isEditable p b && p `Map.notMember` grid

-- | Displaces the machine at the first argument by the second argument
displaceMachineAt :: Point -> Point -> Blueprint -> Blueprint
displaceMachineAt p d b =
  case getMachineAt p b of
    Nothing -> b
    Just m ->
      let b' = removeMachineAt p b
          b'' = placeMachineAt (p +>> d) m b'
       in if b == b' || b' == b'' || b == b'' then b else b''

-- | Utility for getting all points that are in the bounds of the board
allPoints :: Blueprint -> [Point]
allPoints b = [Point x y | x <- [0 .. width b - 1], y <- [0 .. height b - 1]]

getMachineAt :: Point -> Blueprint -> Maybe Machine
getMachineAt p Blueprint {grid = grid} = Map.lookup p grid

placeMachineAt :: Point -> Machine -> Blueprint -> Blueprint
placeMachineAt p m b@Blueprint {grid = grid}
  | not $ isEditable p b = b
  | any (\p' -> not $ isAvailable (p +>> p') b) (allOccupied m) = b
  | m == Occupied = b
  | otherwise = b {grid = newGrid grid}
  where
    newGrid :: Grid Machine -> Grid Machine
    newGrid g =
      let changes = Map.insert p m : map hof (allOccupied m)
          hof = \p' -> Map.insert (p +>> p') Occupied
       in foldr ($) g changes

removeMachineAt :: Point -> Blueprint -> Blueprint
removeMachineAt p b@Blueprint {grid = grid}
  | not $ isEditable p b = b
  | isAvailable p b = b
  | otherwise =
    case getMachineAt p b of
      Nothing -> b
      Just m
        | m == Occupied -> b
        | otherwise -> b {grid = newGrid m grid}
  where
    newGrid :: Machine -> Grid Machine -> Grid Machine
    newGrid m g =
      let changes = Map.delete p : map hof (allOccupied m)
          hof = \p' -> Map.delete (p +>> p')
       in foldr ($) g changes

-- | Returns `True` iff the resources meet the goal specified by the blueprint
isSatisfied :: Blueprint -> Resources -> Bool
isSatisfied Blueprint {grid = grid, minimumSinksToSatisfy = n} r =
  sum (map (`aux` r) $ Map.toList grid) >= n
  where
    aux (p, Sink x) r = if Just x == evalState (getVert p) r then 1 else 0
    aux (_, _) _ = 0
