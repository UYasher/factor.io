module Blueprint where

import Brick.Types (Location (..))
import Budget
import qualified Data.Map as Map
import qualified Data.Set as Set
import Geometry
import Machine
import ResourceUpdate
import State
import Wire

data Blueprint = Blueprint
  { fixedPoints :: Set.Set Point,
    grid :: Grid Machine,
    minimumSinksToSatisfy :: Int,
    budget :: Budget,
    width :: Int,
    height :: Int
  }
  deriving (Eq, Show)

data CellType = Fixed | Empty | Machine {machine :: Machine}

blankBlueprint :: Int -> Int -> Blueprint
blankBlueprint = Blueprint Set.empty Map.empty 0 zeroBudget

-- Operations to Edit Factories

-- | Returns `True` iff the given point is within the bounds of the blueprint
isInBounds :: Point -> Blueprint -> Bool
isInBounds (Point x y) Blueprint {width = width, height = height} =
  0 <= x && x < width && 0 <= y && y < height

-- | Returns the type of CellType at a point
cellTypeAt :: Blueprint -> Point -> CellType
cellTypeAt b p
  | p `elem` fixedPoints b = Fixed
  | otherwise = maybe Empty Machine (getMachineAt p b)

-- | Adds a fixed point to a blueprint. Internal use only.
addFixedPoint :: Point -> Blueprint -> Blueprint
addFixedPoint p b@Blueprint {fixedPoints = fp} = b {fixedPoints = newSet}
  where
    newSet = Set.insert p fp

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

-- | Makes a `Budget` that exactly describes the given `Blueprint`
makeBudgetFrom :: Blueprint -> Budget
makeBudgetFrom Blueprint {grid = g} = Map.foldr aux zeroBudget g
  where
    aux :: Machine -> Budget -> Budget
    aux m b =
      case machineBudgetField m of
        Nothing -> b
        Just s -> Budget.set s (Budget.get s b + 1) b

-- | Places wires in the blueprint from the "Prewiring" mode in the UI.
-- Assumes that the most recent wire (ie the "end" of the drawn path) is at the
-- front of the list, and the oldest wire (ie the "start" of the drawn path)
-- is at the tail of the list
placePrewiresAt :: [Point] -> Blueprint -> Blueprint
placePrewiresAt (x : y : z : rest) b
  | not (x `adjacentTo` y && y `adjacentTo` z) = b
  | not (isInBounds x b && isInBounds y b && isInBounds z b) = b
  | not $ isEditable y b = b
  | otherwise =
    let recursed = placePrewiresAt (y : z : rest) b
     in case getMachineAt y recursed of
          Just (Wire w) ->
            placeMachineAt y (Wire $ wireFromTo x y z `placeOnto` w) recursed
          Nothing ->
            placeMachineAt y (Wire $ wireFromTo x y z) recursed
          _ -> b
placePrewiresAt _ b = b