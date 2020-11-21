module Lib
  ( someFunc,
  )
where

import qualified Data.Map as Map
import Geometry
import Machine
import FactoryEditing

someFunc :: IO ()
someFunc = putStrLn "someFunc"


type Resources = Grid Char


-- Note: we will define functions like cut and then have a stateMonad which converts them into MachineStates
cutter :: MachineState
cutter = MachineState {runMachineState = \xs -> (cut xs, cutter)}
  where
    cut "w" = ("", "vv")

-- balancerR :: MachineState
-- balancerL :: MachineState
-- pipe :: MachineState

-- | A function that runs one step of the simulation.
-- Takes a factory (ie placement) of machines and placement of resources, 
-- and returns a new factory (since internal machine state may have changed)
-- and a new placement of resources
step :: Factory -> Resources -> (Factory, Resources)
step = undefined


-- need some sort of "Goal" type/win condition checking
