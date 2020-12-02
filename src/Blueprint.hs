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

isInBounds :: Point -> Blueprint -> Bool
isInBounds (Point x y) Blueprint {width = width, height = height} =
  0 <= x && x < width && 0 <= y && y < height

isEditable :: Point -> Blueprint -> Bool
isEditable p b@Blueprint {fixedPoints = fixedPoints} =
  isInBounds p b && p `notElem` fixedPoints

isAvailable :: Point -> Blueprint -> Bool
isAvailable p b@Blueprint {grid = grid} =
  isInBounds p b && p `Map.notMember` grid

displaceMachineAt :: Point -> Point -> Blueprint -> Blueprint
displaceMachineAt p d b =
  case getMachineAt p b of
    Nothing -> b
    Just m ->
      let b' = removeMachineAt p b
          b'' = placeMachineAt (p +>> d) m b
       in if b == b' || b' == b'' || b == b'' then b else b''

getMachineAt :: Point -> Blueprint -> Maybe Machine
getMachineAt p Blueprint {grid = grid} = Map.lookup p grid

placeMachineAt :: Point -> Machine -> Blueprint -> Blueprint
placeMachineAt p m b@Blueprint {grid = grid}
  | not $ isInBounds p b = b
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
  | not $ isInBounds p b = b
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
