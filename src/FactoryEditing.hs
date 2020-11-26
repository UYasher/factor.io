module FactoryEditing where

import qualified Data.Map as Map
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Geometry
import Machine

data Factory = Factory
  { fixedPoints :: Set.Set Point,
    floor :: Grid Machine,
    width :: Int,
    height :: Int
  }

blankFactory :: Int -> Int -> Factory
blankFactory = Factory Set.empty Map.empty

-- Operations to Edit Factories

isInBounds :: Point -> Factory -> Bool
isInBounds p f = undefined

displaceMachineAt :: Point -> Point -> Factory -> Factory
displaceMachineAt = undefined

placeMachineAt :: Point -> Machine -> Factory -> Factory
placeMachineAt = undefined

removeMachineAt :: Point -> Factory -> Factory
removeMachineAt = undefined

-- Operations to step factories

data Resources = Resources {horizontal :: Grid (Maybe Int), vertical :: Grid (Maybe Int)}

step :: Factory -> Resources -> Resources
step = undefined

-- stepN :: Factory a -> Int -> Factory a
-- stepN f 0 = f
-- stepN f n = iterate step f !! n

stepUntilComplete :: Factory -> Resources -> Resources
stepUntilComplete = undefined
