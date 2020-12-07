module BlueprintTests where

import Blueprint
import Control.Monad
import Data.Map as Map
import Data.Maybe as Maybe
import Data.Set as Set
import Factory
import Geometry
import GeometryTests
import Machine
import MachineTests
import Operator
import ResourceUpdate
import Test.QuickCheck
import Wire

blueprintTests :: IO ()
blueprintTests = do
  putStrLn "Running BlueprintTests.hs..."
  aux
  putStrLn ""
  where
    aux = do
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

-- Functions to generate an arbitrary instance for a blueprint
-- All blueprints are solvable

blueprintOfOps :: Gen Blueprint
blueprintOfOps = do
  width <- arbitrary
  height <- arbitrary
  operators <- (arbitrary :: Gen [Operator]) -- maybe we should be making sure there isn't a lot of overcrowding by making this a function of width and height
  let machines = Op <$> operators
  fixed <- Set.fromList <$> boundedPoints width height -- not sure this is needed or if this has undesirable behavior
  let base = blankBlueprint width height
  let placeInBounds = placeMachineAt . wrapInBounds width height
  withMachines <- liftM3 Prelude.foldr (placeInBounds <$> arbitrary) (return base) (return machines)
  return withMachines {fixedPoints = fixed}

openOutputs :: Blueprint -> [Point]
openOutputs b@Blueprint {grid = g, width = w, height = h} = filteredOutputList
  where
    r = stepUntilStableOrN (h * w * 4) (fromJust . makeFactory $ b) emptyResources
    isOp m = case m of
      Op _ -> True
      _ -> False
    ops = Map.filter isOp g
    outputList = concat . Map.elems $ Map.mapWithKey (\k a -> Prelude.map (+>> k) a) (Map.map (opOutputs . op) ops)
    isMakingResource p = isJust $ Map.lookup p (vertical r)
    connectsToWire p = case Map.lookup (p +>> Point 0 (-1)) g of
      Just w@(Wire _) -> connectsToNorth $ direction w
      _ -> False
    filteredOutputList = Prelude.filter (\p -> isMakingResource p && (not . connectsToWire) p) outputList

openInputs :: Blueprint -> [Point]
openInputs b@Blueprint {grid = g, width = w, height = h} = filteredInputList
  where
    r = stepUntilStableOrN (h * w * 4) (fromJust . makeFactory $ b) emptyResources
    isOp m = case m of
      Op _ -> True
      _ -> False
    ops = Map.filter isOp g
    inputList = concat . Map.elems $ Map.mapWithKey (\k a -> Prelude.map (+>> k) a) (Map.map (opInputs . op) ops)
    isGettingResource p = isJust $ Map.lookup (p +>> Point 0 1) (vertical r) -- make sure this detects resources going to input correctly
    filteredInputList = Prelude.filter isGettingResource inputList

placeSinksAtOpenOutputs :: Blueprint -> Blueprint
placeSinksAtOpenOutputs b@Blueprint {width = w, height = h} = Prelude.foldr ($) b placeSinkCmds
  where
    r = stepUntilStableOrN (h * w * 4) (fromJust . makeFactory $ b) emptyResources
    placeSinkCmds = Prelude.map (\p -> placeMachineAt (p +>> Point 0 1) (Source . fromJust . fromJust . Map.lookup p $ vertical r)) (openOutputs b)

addSource :: Point -> Gen Blueprint -> Gen Blueprint
addSource p b = do placeMachineAt p <$> (Source <$> arbitrary) <*> b

connectOutputToInput :: Blueprint -> Blueprint
connectOutputToInput = undefined

connectBlueprint :: Gen Blueprint -> Gen Blueprint
connectBlueprint b = do
  b' <- b
  case openOutputs b' of
    x : xs -> case openInputs b' of
      _ : _ ->
        oneof
          [ placeSinksAtOpenOutputs <$> b, -- Not sure these two methods make sense
            connectBlueprint $ connectOutputToInput <$> b
          ]
      [] -> placeSinksAtOpenOutputs <$> b
    [] -> case openInputs b' of
      p : _ -> connectBlueprint (addSource p b) -- Don't we actually want a random point?
      [] -> b

fixSinksAndSources :: Blueprint -> Blueprint
fixSinksAndSources b@Blueprint {grid = g, fixedPoints = ps} = b {fixedPoints = ps'}
  where
    ps' = Set.union ps (Set.fromList sinkAndSourcePoints)
    sinkAndSourcePoints = Map.keys $ Map.filter isSinkOrSource g
    isSinkOrSource m = case m of
      Source _ -> True
      Sink _ -> True
      _ -> False

removeUnfixed :: Blueprint -> Blueprint
removeUnfixed b@Blueprint {grid = g, fixedPoints = ps} = b {grid = g'}
  where
    g' = Map.filterWithKey (\p _ -> p `elem` ps) g

getNumSinks :: Blueprint -> Int
getNumSinks Blueprint {grid = g} = Map.foldr (+) 0 $ Map.map sinkToInt g
  where
    sinkToInt p = case p of
      Sink _ -> 1
      _ -> 0

instance Arbitrary Blueprint where
  arbitrary = do
    b <- removeUnfixed . fixSinksAndSources <$> connectBlueprint blueprintOfOps
    n <- choose (0, getNumSinks b)
    return b {minimumSinksToSatisfy = n}

-- Utility functions for asking
allPoints :: Blueprint -> [Point]
allPoints b = [Point x y | x <- [0 .. width b - 1], y <- [0 .. height b - 1]]

blueprintWithArea :: Gen Blueprint
blueprintWithArea = arbitrary `suchThat` (not . Prelude.null . allPoints)

blueprintWithMachines :: Gen Blueprint
blueprintWithMachines = arbitrary `suchThat` aux
  where
    aux b = any (\p -> isJust $ getMachineAt p b) $ allPoints b

pointWithMachine :: Blueprint -> Gen Point
pointWithMachine b = elements (Prelude.filter (\p -> isJust $ getMachineAt p b) $ allPoints b)

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

prop_isAvailableBeforeRemove :: Gen Property
prop_isAvailableBeforeRemove = do
  b <- blueprintWithArea
  p <- elements $ allPoints b
  return $ isAvailable p b ==> b == removeMachineAt p b

prop_isAvailableBeforeDisplace :: Point -> Gen Property
prop_isAvailableBeforeDisplace d = do
  b <- blueprintWithArea
  p <- elements $ allPoints b
  return $ isAvailable p b ==> b == displaceMachineAt p d b

prop_placeDoesntOverlap :: Machine -> Gen Property
prop_placeDoesntOverlap m = do
  b <- blueprintWithArea
  p <- elements $ allPoints b
  return $
    placeMachineAt p m b /= b
      ==> all (\p' -> isAvailable (p' +>> p) b) (allOccupied m)

prop_isAvailableAfterPlace :: Machine -> Gen Property
prop_isAvailableAfterPlace m = do
  b <- blueprintWithArea
  p <- elements $ allPoints b
  let b' = placeMachineAt p m b
  return $ b' /= b ==> not $ isAvailable p b'

prop_isAvailableAfterRemove :: Gen Property
prop_isAvailableAfterRemove = do
  b <- blueprintWithMachines
  p <- pointWithMachine b
  let b' = removeMachineAt p b
  return $ b' /= b ==> isAvailable p b'

prop_isAvailableAfterDisplace :: Point -> Gen Property
prop_isAvailableAfterDisplace d = do
  b <- blueprintWithMachines
  p <- pointWithMachine b
  let b' = displaceMachineAt p d b
  return $
    b' /= b ==> isAvailable p b'
      || ( case getMachineAt p b of
             Nothing -> False
             Just Occupied -> True
             Just m -> Geometry.negate d `elem` allOccupied m
         )

prop_placeDidPlace :: Machine -> Gen Property
prop_placeDidPlace m = do
  b <- blueprintWithArea
  p <- elements $ allPoints b
  let b' = placeMachineAt p m b
  return $ b /= b' ==> Just m == getMachineAt p b'

prop_removeDidRemove :: Gen Property
prop_removeDidRemove = do
  b <- blueprintWithMachines
  p <- pointWithMachine b
  let b' = removeMachineAt p b
  case getMachineAt p b of
    Nothing -> return $ False ==> True
    Just Occupied -> return $ False ==> True
    Just m -> return $ True ==> all (\p' -> isAvailable (p +>> p') b') (allOccupied m)

prop_placeRemove :: Machine -> Gen Property
prop_placeRemove m = do
  b <- blueprintWithArea
  p <- elements $ allPoints b
  let b' = placeMachineAt p m b
  let b'' = removeMachineAt p b'
  return $ b /= b' ==> b == b''

prop_removePlace :: Gen Property
prop_removePlace = do
  b <- blueprintWithMachines
  p <- pointWithMachine b
  let m = getMachineAt p b
  let b' = removeMachineAt p b
  let b'' = (\m' -> placeMachineAt p m' b') <$> m
  return $ isJust m ==> b'' == Just b

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