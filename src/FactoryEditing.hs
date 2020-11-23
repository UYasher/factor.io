module FactoryEditing where

import qualified Data.Map as Map
import Data.Maybe (isJust)
import Geometry
import Machine

data Factory a = Factory {permanentFloor :: Grid (Machine a), editableFloor :: Grid (Machine a), width :: Int, height :: Int}

-- Operations to Edit Factories

blankFactory :: Int -> Int -> Factory a
blankFactory = Factory Map.empty Map.empty

isInBounds :: Point -> Factory a -> Bool
isInBounds p f = undefined

rotateMachineAt :: Point -> Orientation -> Factory a -> Factory a
rotateMachineAt = undefined

displaceMachineAt :: Point -> Point -> Factory a -> Factory a
displaceMachineAt = undefined

placeMachineAt :: Point -> Machine a -> Factory a -> Factory a
placeMachineAt = undefined

removeMachineAt :: Point -> Factory a -> Factory a
removeMachineAt = undefined

-- Operations to step factories

step :: Factory a -> Factory a
step = undefined

stepN :: Factory a -> Int -> Factory a
stepN f 0 = f
stepN f n = iterate step f !! n

-- Operations to solve factories

type Solver a = Factory a -> Maybe (Factory a, Int)

-- Check if a factory is solved
isSolved :: Factory a -> Bool
isSolved = undefined

-- Any Int puzzle can be solved by going to a prime, factoring, producing a one, and then repeatedly adding
-- There is a minimum amount of space required for that structure
-- If such a structure exists, return the solution factory and the number of steps required to reach a solution
-- Note it might be possible to come up with a better simple solution with BFS on a dependency graph
findSimpleSolution :: Solver a
findSimpleSolution = undefined

isSimpleSolvable :: Factory a -> Bool
isSimpleSolvable f = isJust $ findSimpleSolution f

-- Some problems can be solved more efficiently than the simple solution
-- It might be possible to improve those solutions using local search
-- The generated solution might not be optimal
findComplexSolution :: Solver a
findComplexSolution = undefined

isComplexSolvable :: Factory a -> Bool
isComplexSolvable f = isJust $ findComplexSolution f

-- Generates the solution to a puzzle which uses the fewest machines
-- If local search (or other alternatives for implementing findComplexSolution)
-- Do not return an optimal solution, then we may use constraint satisfaction
findOptimalSolution :: Solver a
findOptimalSolution = undefined

isSolvable :: Factory a -> Bool
isSolvable f = isJust $ findOptimalSolution f