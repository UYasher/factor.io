module FactoryEditing where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Geometry
import Machine

data Factory = Factory
  { fixedPoints :: Set.Set Point,
    grid :: Grid Machine,
    width :: Int,
    height :: Int
  }
  deriving (Eq, Show)

blankFactory :: Int -> Int -> Factory
blankFactory = Factory Set.empty Map.empty

-- Operations to Edit Factories

isInBounds :: Point -> Factory -> Bool
isInBounds (Point x y) Factory {width = width, height = height} =
  0 <= x && x < width && 0 <= y && y < height

isEditable :: Point -> Factory -> Bool
isEditable p f@Factory {fixedPoints = fixedPoints} =
  isInBounds p f && p `notElem` fixedPoints

isAvailable :: Point -> Factory -> Bool
isAvailable p f@Factory {grid = grid} =
  isInBounds p f && p `Map.notMember` grid

displaceMachineAt :: Point -> Point -> Factory -> Factory
displaceMachineAt p d f =
  case getMachineAt p f of
    Nothing -> f
    Just m ->
      let f' = removeMachineAt p f
          f'' = placeMachineAt (p +>> d) m f
       in if f == f' || f' == f'' || f == f'' then f else f''

getMachineAt :: Point -> Factory -> Maybe Machine
getMachineAt p Factory {grid = grid} = Map.lookup p grid

placeMachineAt :: Point -> Machine -> Factory -> Factory
placeMachineAt p m f@Factory {grid = grid}
  | not $ isInBounds p f = f
  | not $ isEditable p f = f
  | any (\p' -> not $ isAvailable (p +>> p') f) (allOccupied m) = f
  | m == Occupied = f
  | otherwise = f {grid = newGrid grid}
  where
    newGrid :: Grid Machine -> Grid Machine
    newGrid g =
      let changes = Map.insert p m : map hof (allOccupied m)
          hof = \p' -> Map.insert (p +>> p') Occupied
       in foldr ($) g changes

removeMachineAt :: Point -> Factory -> Factory
removeMachineAt p f@Factory {grid = grid}
  | not $ isInBounds p f = f
  | not $ isEditable p f = f
  | isAvailable p f = f
  | otherwise =
    case getMachineAt p f of
      Nothing -> f
      Just m
        | m == Occupied -> f
        | otherwise -> f {grid = newGrid m grid}
  where
    newGrid :: Machine -> Grid Machine -> Grid Machine
    newGrid m g =
      let changes = Map.delete p : map hof (allOccupied m)
          hof = \p' -> Map.delete (p +>> p')
       in foldr ($) g changes

-- Operations to step factories

data Resources = Resources {horizontal :: Grid (Maybe Int), vertical :: Grid (Maybe Int)}

step :: Factory -> Resources -> Resources
step = undefined

-- stepN :: Factory a -> Int -> Factory a
-- stepN f 0 = f
-- stepN f n = iterate step f !! n

stepUntilComplete :: Factory -> Resources -> Resources
stepUntilComplete = undefined
