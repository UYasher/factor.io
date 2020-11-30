module BlueprintTests where

import Blueprint
import Control.Monad
import Data.Map as Map
import Data.Maybe as Maybe
import Data.Set as Set
import Geometry
import GeometryTests
import Machine
import MachineTests
import Test.QuickCheck

blueprintTests :: IO ()
blueprintTests = do
  putStrLn "Running BlueprintTests.hs..."
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
instance Arbitrary Blueprint where
  arbitrary = do
    width <- arbitrary
    height <- arbitrary
    machines <- arbitrary :: Gen [Machine]
    fixed <- Set.fromList <$> (arbitrary :: Gen [Point])
    let base = blankBlueprint width height
    withMachines <- liftM3 Prelude.foldr (placeMachineAt <$> arbitrary) (return base) (return machines)
    return withMachines {fixedPoints = fixed}

prop_nonEditable :: Point -> Point -> Machine -> Blueprint -> Property
prop_nonEditable p d m b =
  not (isEditable p b)
    ==> placeMachineAt p m b == b
    && removeMachineAt p b == b
    && displaceMachineAt p d b == b

prop_outOfBoundsIsNotEditable :: Point -> Blueprint -> Property
prop_outOfBoundsIsNotEditable p b = not (isInBounds p b) ==> not (isEditable p b)

prop_outOfBoundsIsNotAvailable :: Point -> Blueprint -> Property
prop_outOfBoundsIsNotAvailable p b = not (isInBounds p b) ==> not (isAvailable p b)

prop_isAvailableBeforePlace :: Point -> Machine -> Blueprint -> Property
prop_isAvailableBeforePlace p m b =
  not (isAvailable p b) ==> b == placeMachineAt p m b

prop_isAvailableBeforeRemove :: Point -> Blueprint -> Property
prop_isAvailableBeforeRemove p b = isAvailable p b ==> b == removeMachineAt p b

prop_isAvailableBeforeDisplace :: Point -> Point -> Blueprint -> Property
prop_isAvailableBeforeDisplace p d b = isAvailable p b ==> b == displaceMachineAt p d b

prop_placeDoesntOverlap :: Point -> Machine -> Blueprint -> Property
prop_placeDoesntOverlap p m b =
  placeMachineAt p m b /= b
    ==> all (\p' -> isAvailable (p' +>> p) b) (allOccupied m)

prop_isAvailableAfterPlace :: Point -> Machine -> Blueprint -> Property
prop_isAvailableAfterPlace p m b =
  let b' = placeMachineAt p m b
   in b' /= b ==> not $ isAvailable p b'

prop_isAvailableAfterRemove :: Point -> Blueprint -> Property
prop_isAvailableAfterRemove p b =
  let b' = removeMachineAt p b
   in b' /= b ==> isAvailable p b'

prop_isAvailableAfterDisplace :: Point -> Point -> Blueprint -> Property
prop_isAvailableAfterDisplace p d b =
  let b' = displaceMachineAt p d b
   in b' /= b ==> isAvailable p b'
        || ( case getMachineAt p b of
               Nothing -> False
               Just m -> Geometry.negate d `elem` Prelude.map (+>> p) (allOccupied m)
           )

prop_placeDidPlace :: Point -> Machine -> Blueprint -> Property
prop_placeDidPlace p m b =
  let b' = placeMachineAt p m b
   in b /= b' ==> Just m == getMachineAt p b'

prop_removeDidRemove :: Point -> Blueprint -> Property
prop_removeDidRemove p b =
  let b' = removeMachineAt p b
   in case getMachineAt p b of
        Nothing -> False ==> True
        Just m -> True ==> all (\p' -> isAvailable (p +>> p') b') (allOccupied m)

prop_placeRemove :: Point -> Machine -> Blueprint -> Property
prop_placeRemove p m b =
  let b' = placeMachineAt p m b
      b'' = removeMachineAt p b'
   in b /= b' ==> b == b''

prop_removePlace :: Point -> Blueprint -> Property
prop_removePlace p b =
  let m = getMachineAt p b
      b' = removeMachineAt p b
      b'' = (\m' -> placeMachineAt p m' b') <$> m
   in isJust m ==> b'' == Just b

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