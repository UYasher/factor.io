module FactoryEditing where

import Geometry
import Machine
import qualified Data.Map as Map

data Factory = Factory {permanentFloor :: Grid Machine, editableFloor :: Grid Machine, width :: Int, height :: Int}

blankFactory :: Int -> Int -> Factory
blankFactory = Factory Map.empty Map.empty

isInBounds :: Point -> Factory -> Bool
isInBounds p f = undefined

rotateMachineAt :: Point -> Orientation -> Factory -> Factory
rotateMachineAt = undefined

displaceMachineAt :: Point -> Point -> Factory -> Factory
displaceMachineAt = undefined

placeMachineAt :: Point -> Machine -> Factory -> Factory
placeMachineAt = undefined

removeMachineAt :: Point -> Factory -> Factory
removeMachineAt = undefined


