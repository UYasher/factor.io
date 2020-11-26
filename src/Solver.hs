module Solver where

-- Operations to solve factories

-- type Solver a = Factory a -> Maybe (Factory a, Int)

-- -- Check if a factory is solved
-- isSolved :: Factory a -> Bool
-- isSolved = undefined

-- -- Any Int puzzle can be solved by going to a prime, factoring, producing a one, and then repeatedly adding
-- -- There is a minimum amount of space required for that structure
-- -- If such a structure exists, return the solution factory and the number of steps required to reach a solution
-- -- Note it might be possible to come up with a better simple solution with BFS on a dependency graph
-- findSimpleSolution :: Solver a
-- findSimpleSolution = undefined

-- isSimpleSolvable :: Factory a -> Bool
-- isSimpleSolvable f = isJust $ findSimpleSolution f

-- -- Some problems can be solved more efficiently than the simple solution
-- -- It might be possible to improve those solutions using local search
-- -- The generated solution might not be optimal
-- findComplexSolution :: Solver a
-- findComplexSolution = undefined

-- isComplexSolvable :: Factory a -> Bool
-- isComplexSolvable f = isJust $ findComplexSolution f

-- -- Generates the solution to a puzzle which uses the fewest machines
-- -- If local search (or other alternatives for implementing findComplexSolution)
-- -- Do not return an optimal solution, then we may use constraint satisfaction
-- findOptimalSolution :: Solver a
-- findOptimalSolution = undefined

-- isSolvable :: Factory a -> Bool
-- isSolvable f = isJust $ findOptimalSolution f