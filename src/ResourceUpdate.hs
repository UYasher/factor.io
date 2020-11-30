module ResourceUpdate (Resources (Resources), getHoriz, getVert, setHoriz, setVert, emptyResources) where

import Control.Monad
import Data.Map as Map
import Geometry
import State

data Resources = Resources {horizontal :: Grid (Maybe Int), vertical :: Grid (Maybe Int)}
  deriving (Eq, Show)

emptyResources :: Resources
emptyResources = Resources Map.empty Map.empty

getHoriz :: Point -> State Resources (Maybe Int)
getHoriz p = do
  Resources {horizontal = g} <- get
  return . join $ Map.lookup p g

getVert :: Point -> State Resources (Maybe Int)
getVert p = do
  Resources {vertical = g} <- get
  return . join $ Map.lookup p g

setHoriz :: Point -> Maybe Int -> State Resources ()
setHoriz p m = do
  modify (\r@Resources {horizontal = g} -> newR r g)
  where
    newR r g = r {horizontal = Map.insert p m g}

setVert :: Point -> Maybe Int -> State Resources ()
setVert p m = do
  modify (\r@Resources {vertical = g} -> newR r g)
  where
    newR r g = r {vertical = Map.insert p m g}