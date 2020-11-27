module FactoryEditingTests where

import Control.Monad
import Data.Map as Map
import Data.Maybe as Maybe
import Data.Set as Set
import FactoryEditing
import Geometry
import GeometryTests
import Machine
import MachineTests
import Test.QuickCheck

factoryEditingTests :: IO ()
factoryEditingTests = do
  putStrLn "Running FactoryEditingTests.hs..."
  aux
  putStrLn ""
  where
    aux = do
      -- putStrLn "unimplemented"

      quickCheck prop_nonEditable
      quickCheck prop_outOfBoundsIsNotEditable
      quickCheck prop_outOfBoundsIsNotAvailable
      quickCheck prop_isAvailableBeforePlace
      quickCheck prop_isAvailableBeforeRemove
      quickCheck prop_isAvailableBeforeDisplace
      quickCheck prop_placeDoesntOverlap
      quickCheck prop_isAvailableAfterPlace
      quickCheck prop_isAvailableAfterRemove
      quickCheck prop_isAvailableAfterDisplace
      quickCheck prop_placeDidPlace
      quickCheck prop_removeDidRemove
      quickCheck prop_placeRemove
      quickCheck prop_removePlace

-- Need arbitrary instance for factories -- need to be sophisticated about this
instance Arbitrary Factory where
  arbitrary = do
    width <- arbitrary
    height <- arbitrary
    machines <- arbitrary :: Gen [Machine]
    fixed <- Set.fromList <$> (arbitrary :: Gen [Point])
    let base = blankFactory width height
    withMachines <- liftM3 Prelude.foldr (placeMachineAt <$> arbitrary) (return base) (return machines)
    return withMachines {fixedPoints = fixed}

prop_nonEditable :: Point -> Point -> Machine -> Factory -> Property
prop_nonEditable p d m f =
  not (isEditable p f)
    ==> placeMachineAt p m f == f
    && removeMachineAt p f == f
    && displaceMachineAt p d f == f

prop_outOfBoundsIsNotEditable :: Point -> Factory -> Property
prop_outOfBoundsIsNotEditable p f = not (isInBounds p f) ==> not (isEditable p f)

prop_outOfBoundsIsNotAvailable :: Point -> Factory -> Property
prop_outOfBoundsIsNotAvailable p f = not (isInBounds p f) ==> not (isAvailable p f)

prop_isAvailableBeforePlace :: Point -> Machine -> Factory -> Property
prop_isAvailableBeforePlace p m f =
  not (isAvailable p f) ==> f == placeMachineAt p m f

prop_isAvailableBeforeRemove :: Point -> Factory -> Property
prop_isAvailableBeforeRemove p f = isAvailable p f ==> f == removeMachineAt p f

prop_isAvailableBeforeDisplace :: Point -> Point -> Factory -> Property
prop_isAvailableBeforeDisplace p d f = isAvailable p f ==> f == displaceMachineAt p d f

prop_placeDoesntOverlap :: Point -> Machine -> Factory -> Property
prop_placeDoesntOverlap p m f =
  placeMachineAt p m f /= f
    ==> all (\p' -> isAvailable (p' +>> p) f) (allOccupied m)

prop_isAvailableAfterPlace :: Point -> Machine -> Factory -> Property
prop_isAvailableAfterPlace p m f =
  let f' = placeMachineAt p m f
   in f' /= f ==> not $ isAvailable p f'

prop_isAvailableAfterRemove :: Point -> Factory -> Property
prop_isAvailableAfterRemove p f =
  let f' = removeMachineAt p f
   in f' /= f ==> isAvailable p f'

prop_isAvailableAfterDisplace :: Point -> Point -> Factory -> Property
prop_isAvailableAfterDisplace p d f =
  let f' = displaceMachineAt p d f
   in f' /= f ==> isAvailable p f'
        || ( case getMachineAt p f of
               Nothing -> False
               Just m -> Geometry.negate d `elem` Prelude.map (+>> p) (allOccupied m)
           )

prop_placeDidPlace :: Point -> Machine -> Factory -> Property
prop_placeDidPlace p m f =
  let f' = placeMachineAt p m f
   in f /= f' ==> Just m == getMachineAt p f'

prop_removeDidRemove :: Point -> Factory -> Property
prop_removeDidRemove p f =
  let f' = removeMachineAt p f
   in case getMachineAt p f of
        Nothing -> False ==> True
        Just m -> True ==> all (\p' -> isAvailable (p +>> p') f') (allOccupied m)

prop_placeRemove :: Point -> Machine -> Factory -> Property
prop_placeRemove p m f =
  let f' = placeMachineAt p m f
      f'' = removeMachineAt p f'
   in f /= f' ==> f == f''

prop_removePlace :: Point -> Factory -> Property
prop_removePlace p f =
  let m = getMachineAt p f
      f' = removeMachineAt p f
      f'' = (\m' -> placeMachineAt p m' f') <$> m
   in isJust m ==> f'' == Just f

-- -- Tests to check factory editing operations
-- prop_place :: Point -> Machine a -> Factory a -> Bool
-- prop_place p m f = Map.lookup p (placeMachineAt p f m) == Just m

-- prop_remove :: Point -> Factory a -> Bool
-- prop_remove p f = isNothing $ Map.lookup p (removeMachineAt p f)

-- prop_placeRemove :: Point -> Machine a -> Bool
-- prop_placeRemove p m = removeMachineAt p . placeMachineAt p m == id

-- prop_placeDisplace :: Point -> Point -> Machine a -> Factory a -> Bool
-- prop_placeDisplace p1 p2 m f =
--     let f' = displaceMachineAt p1 p2 (placeMachineAt p1 m f)
--     in Map.lookup (p1 + p2) f'  == Just m

-- prop_removePlace :: Point -> Machine a -> Factory a
-- prop_removePlace p m f =
--     let f' = placeMachineAt p m (removeMachineAt p f)
--     in Map.lookup p f' == Just m

-- prop_removeDisplace :: Point -> Point -> Bool
-- prop_removeDisplace p1 p2 = displaceMachineAt p1 p2 . removeMachineAt p1 == id

-- prop_displaceRemove :: Point -> Point -> Bool
-- prop_displaceRemove p1 p2 = removeMachineAt p1 . displaceMachineAt p1 p2 == displaceMachineAt p1 p2

-- prop_displaceRemainsInBounds :: Point -> Point -> Property
-- prop_displaceRemainsInBounds p1 p2 = notInBounds (p1 + p2) ==> displaceMachineAt p1 p2 == id

-- prop_placeRotate :: Point -> Orientation -> Factory a -> Bool
-- prop_placeRotate p o = placeMachineAt p (rotateMachine o) == rotateMachineAt p o

-- prop_removeRotate :: Point -> Orientation -> Bool
-- prop_removeRotate p o = removeMachineAt p . rotateMachineAt p o == rotateMachineAt p o . removeMachineAt p

-- prop_displaceRotate :: Point -> Point -> Orientation -> Bool
-- prop_displaceRotate p1 p2 o = displaceMachineAt p1 p2 (rotateMachine o) == rotateMachineAt (p1 + p2) o (displaceMachineAt p1 p2)

-- -- Tests to check factory solvability

-- prop_solutionIsSolvable :: Solver a -> Factory a -> Bool
-- prop_solutionIsSolvable p f = case p f of
--     Nothing -> True
--     Just (f' n) -> isSolved (stepN f' n)

-- prop_simpleSolutionIsSolvable :: Factory a -> Bool
-- prop_simpleSolutionIsSolvable = prop_solutionIsSolvable findSimpleSolution

-- prop_copmlexSolutionIsSolvable :: Factory a -> Bool
-- prop_copmlexSolutionIsSolvable = prop_solutionIsSolvable findComplexSolution

-- prop_optimalSolutionIsSolvable :: Factory a -> Bool
-- prop_copmlexSolutionIsSolvable = prop_solutionIsSolvable findOptimalSolution

-- -- Establishes a partial ordering on solvers
-- (*<=) :: Solver a -> Solver a -> (Factory a -> Bool)
-- s1 (*<=) s2 = \f -> case s2 f of
--     Nothing -> True
--     Just (_, n2) -> case s1 of
--         Nothing -> False
--         Just (_, n1) -> n1 <= n2

-- prop_complexLESimple :: Factory a -> Bool
-- prop_complexLessThanSimple = findComplexSolution *<= findSimpleSolution

-- prop_optimalLESimple :: Factory a -> Bool
-- prop_optimalLessThanSimple = findOptimalSolution *<= findSimpleSolution

-- prop_optimalLEComplex :: Factory a -> Bool
-- prop_complexLessThanSimple = findOptimalSolution *<= findComplexSolution