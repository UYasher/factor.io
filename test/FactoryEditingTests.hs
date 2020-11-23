module FactoryEditingTests where
import Test.QuickCheck
import FactoryEditing
import Data.Map as Map

-- Need arbitrary instance for factories -- need to be sophisticated about this
instance Arbitrary Factory where
    arbitrary = undefined
    shrink = undefined

-- Need arbitrary instance for machines -- this will be a choose, might need to be a little smarter about shrink
instance Arbitrary Machine where
    arbitrary = undefined
    shrink = undefined


-- Tests to check factory editing operations
prop_place :: Point -> Machine -> Factory -> Bool
prop_place p m f = Map.lookup p (placeMachineAt p f m) == Just m

prop_remove :: Point -> Factory -> Bool
prop_remove p f = isNothing $ Map.lookup p (removeMachineAt p f)

prop_placeRemove :: Point -> Machine -> Bool
prop_placeRemove p m = removeMachineAt p . placeMachineAt p m == id

prop_placeDisplace :: Point -> Point -> Machine -> Factory -> Bool
prop_placeDisplace p1 p2 m f =
    let f' = displaceMachineAt p1 p2 (placeMachineAt p1 m f)
    in Map.lookup (p1 + p2) f'  == Just m

prop_removePlace :: Point -> Machine -> Factory
prop_removePlace p m f =
    let f' = placeMachineAt p m (removeMachineAt p f)
    in Map.lookup p f' == Just m

prop_removeDisplace :: Point -> Point -> Bool
prop_removeDisplace p1 p2 = displaceMachineAt p1 p2 . removeMachineAt p1 == id

prop_displaceRemove :: Point -> Point -> Bool
prop_displaceRemove p1 p2 = removeMachineAt p1 . displaceMachineAt p1 p2 == displaceMachineAt p1 p2

prop_displaceRemainsInBounds :: Point -> Point -> Property
prop_displaceRemainsInBounds p1 p2 = notInBounds (p1 + p2) ==> displaceMachineAt p1 p2 == id

prop_placeRotate :: Point -> Orientation -> Factory -> Bool
prop_placeRotate p o = placeMachineAt p (rotateMachine o) == rotateMachineAt p o

prop_removeRotate :: Point -> Orientation -> Bool
prop_removeRotate p o = removeMachineAt p . rotateMachineAt p o == rotateMachineAt p o . removeMachineAt p

prop_displaceRotate :: Point -> Point -> Orientation -> Bool
prop_displaceRotate p1 p2 o = displaceMachineAt p1 p2 (rotateMachine o) == rotateMachineAt (p1 + p2) o (displaceMachineAt p1 p2)


-- Tests to check factory solvability

prop_solutionIsSolvable :: Solver -> Factory -> Bool
prop_solutionIsSolvable p f = case p f of
    Nothing -> True
    Just (f' n) -> isSolved (stepN f' n)

prop_simpleSolutionIsSolvable :: Factory -> Bool
prop_simpleSolutionIsSolvable = prop_solutionIsSolvable findSimpleSolution

prop_copmlexSolutionIsSolvable :: Factory -> Bool
prop_copmlexSolutionIsSolvable = prop_solutionIsSolvable findComplexSolution

prop_optimalSolutionIsSolvable :: Factory -> Bool
prop_copmlexSolutionIsSolvable = prop_solutionIsSolvable findOptimalSolution

-- Establishes a partial ordering on solvers
(*<=) :: Solver -> Solver -> (Factory -> Bool)
s1 (*<=) s2 = \f -> case s2 f of
    Nothing -> True
    Just (_, n2) -> case s1 of
        Nothing -> False
        Just (_, n1) -> n2 <= n1

prop_complexLESimple :: Factory -> Bool
prop_complexLessThanSimple = findComplexSolution *<= findSimpleSolution

prop_optimalLESimple :: Factory -> Bool
prop_optimalLessThanSimple = findOptimalSolution *<= findSimpleSolution

prop_optimalLEComplex :: Factory -> Bool
prop_complexLessThanSimple = findOptimalSolution *<= findComplexSolution