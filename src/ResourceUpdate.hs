module ResourceUpdate where

import Control.Monad (join)
import Data.Map as Map
import Geometry
import State

-- | `Resources` keeps track of which values are "flowing" through which wires.
-- It has a horizontal layer and a vertical layer, due to the behavior of how
-- exactly wires work.
data Resources = Resources {horizontal :: Grid (Maybe Int), vertical :: Grid (Maybe Int)} -- I'm not sure why this has grids of `Maybe Int` wouldn't grids of `Int` suffice?
  deriving (Eq, Show)

-- | A `Resources` with no values in it
emptyResources :: Resources
emptyResources = Resources Map.empty Map.empty

-- | Returns a `State` type that when run on `Resources` returns `Just` the value
-- in the horizontal layer at the argument, or `Nothing`
getHoriz :: Point -> State Resources (Maybe Int)
getHoriz p = do
  Resources {horizontal = g} <- get
  return . join $ Map.lookup p g

-- | Returns a `State` type that when run on `Resources` returns `Just` the value
-- in the vertical layer at the argument, or `Nothing`
getVert :: Point -> State Resources (Maybe Int)
getVert p = do
  Resources {vertical = g} <- get
  return . join $ Map.lookup p g

-- | Returns a `State` type that when run on `Resources` sets the value in the
-- horizontal layer at the first argument to be the second argument
setHoriz :: Point -> Maybe Int -> State Resources ()
setHoriz p m = do
  modify (\r@Resources {horizontal = g} -> newR r g)
  where
    newR r g = r {horizontal = Map.insert p m g}

-- | Returns a `State` type that when run on `Resources` sets the value in the
-- vertical layer at the first argument to be the second argument
setVert :: Point -> Maybe Int -> State Resources ()
setVert p m = do
  modify (\r@Resources {vertical = g} -> newR r g)
  where
    newR r g = r {vertical = Map.insert p m g}