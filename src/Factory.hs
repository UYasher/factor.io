module Factory (Factory, makeFactory, step, isSatisfied) where

import Blueprint
import Control.Monad
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Geometry
import Machine
import Operator
import ResourceUpdate
import State
import Wire

type Factory = State Resources ()

-- | Converts a `Blueprint` into `Just` a `Factory`, or `Nothing` in the case of
-- an illegal wire configuration
makeFactory :: Blueprint -> Maybe Factory
makeFactory Blueprint {grid = g} =
  processWires g >>= Just . (>> processNonWires g)

-- | Temporary data type that helps talk about a specific place in a Resource map
data Coord = Horiz Point | Vert Point deriving (Eq, Ord)

-- | Returns `True` iff the argument refers to horizontal current/resources
isHoriz :: Coord -> Bool
isHoriz (Horiz _) = True
isHoriz (Vert _) = False

-- | Converts the argument into a `get` at the specified point, in either the
-- | horizontal or the vertical resource layer
coordToGet :: Coord -> State Resources (Maybe Int)
coordToGet (Horiz p) = getHoriz p
coordToGet (Vert p) = getVert p

-- | Converts the argument into a `set` at the specified point, in either the
-- | horizontal or the vertical resource layer
coordToSet :: Coord -> Maybe Int -> State Resources ()
coordToSet (Horiz p) = setHoriz p
coordToSet (Vert p) = setVert p

-- | Does "syntax checking" on a grid of machines. Returns `Just` a `Factory` if
-- the wire arrangement is valid, and `Nothing` if there is an error
processWires :: Grid Machine -> Maybe Factory
-- First, we generate a list of "wire snakes" (a path of contiguous wire segments
-- that starts in a valid configuration). Then, we check to see if any two wire
-- snakes assigned current to the same point in the same layer. If they did,
-- there's a wire snake that connects two outputs of machines, which is illegal.
-- Otherwise, we can sequence together the "factory" of each wire snake
processWires g =
  let snakes =
        mapMaybe
          ( \p ->
              if isOutput g p
                then Just $ processWireSnake g p
                else Nothing
          )
          (Map.keys g)
   in if anySame (snakes >>= snd) then Nothing else Just $ mapM_ fst snakes
  where
    -- Returns `True` iff the given point could be the start of a wire snake
    -- (ie, is a Source or is in the output of some Operator)
    isOutput :: Grid Machine -> Point -> Bool
    isOutput g p =
      any
        ( \(p', m) -> case m of
            Op op -> p `elem` map (+>> p') (outputs op)
            Source _ -> p == p'
            _ -> False
        )
        $ Map.toList g
    anySame :: Ord a => [a] -> Bool
    anySame xs = Set.size (Set.fromList xs) < length xs

-- | Converts the wire snake starting at the given point into a `Factory` that
-- makes current flow from the head of the snake to the tail. Also returns the
-- points and layers current was assigned to, to aid illegal wire configuration detection.
processWireSnake :: Grid Machine -> Point -> (Factory, [Coord])
processWireSnake g p = aux g p $ p ->> Point 0 1
  where
    -- Recursively follows a wire snake in the grid. The previous point is the
    -- first Point argument. The current point is the second Point argument
    aux :: Grid Machine -> Point -> Point -> (Factory, [Coord])
    aux g prev cur =
      let currentCoord = coordFrom prev cur
       in case Map.lookup cur g of
            Just (Wire dir) ->
              let newPoint = nextPointFrom dir prev cur
               in -- Notice: we create wire delay by sequencing the tail of the snake
                  -- before the head, so that signals can only travel one segment per step
                  combine (aux g cur newPoint) (wireToFactory dir prev cur, currentCoord)
            _ -> (return (), [currentCoord])

    combine :: (Factory, [Coord]) -> (Factory, Coord) -> (Factory, [Coord])
    combine (f1, c1) (f2, c2) = (f1 >> f2, c2 : c1)

    -- Creates a coordinate on `cur`, approached from `prev`
    coordFrom prev cur = case cur ->> prev of
      Point 1 0 -> Horiz cur
      Point (-1) 0 -> Horiz cur
      Point 0 1 -> Vert cur
      Point 0 (-1) -> Vert cur
      _ -> error "impossible programming error"

    -- Converts a wire into a `Factory`, centered on `cur`, approached from `prev`
    wireToFactory dir prev cur
      | currentBends dir = do
        let coord = coordFrom prev cur
        x <- coordToGet coord
        if isHoriz coord then setVert cur x else setHoriz cur x
      | otherwise = do
        let coord = coordFrom prev cur
        x <- coordToGet coord
        if isHoriz coord then setHoriz cur x else setVert cur x

-- | Processes all non-wire machines and converts them into a `Factory`
processNonWires :: Grid Machine -> Factory
processNonWires g = mapM_ aux (Map.toList g)
  where
    aux :: (Point, Machine) -> Factory
    aux (p, Op op) = do
      -- since nothing can put a value on the Occupied representing the input,
      -- we have to read from one above the input
      let globalInputs = map (getVert . (+>> (p +>> Point 0 1))) $ inputs op
      let globalOutputs = map (setVert . (+>> p)) $ outputs op
      mis <- foldr (liftM2 (:)) (return []) globalInputs
      case sequence mis of
        Nothing -> return ()
        Just l ->
          let result = f op l
           in foldr (\(x, y) -> ((x $ Just y) >>)) (return ()) (zip globalOutputs result)
    aux (p, Source v) = do
      setVert p $ Just v
    aux (p, Sink _) = do
      v <- getVert (p +>> Point 0 1)
      setVert p v
    aux (_, Occupied) = return ()
    aux (_, Wire _) = return ()

-- | Runs the factory simulation for one step
step :: Factory -> Resources -> Resources
step = execState

-- | Returns `True` iff the resources meet the goal specified by the blueprint
isSatisfied :: Blueprint -> Resources -> Bool
isSatisfied b r = undefined