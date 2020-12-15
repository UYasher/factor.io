module BlueprintTests where

import Blueprint
import BlueprintParsing
import Control.Monad
import Data.Map as Map
import Data.Maybe as Maybe
import Data.Set as Set
import Factory
import Geometry
import GeometryTests
import GraphUtils
import Machine
import MachineTests
import Operator
import ResourceUpdate
import Test.HUnit
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

getBoardResources :: Blueprint -> Resources
getBoardResources b@Blueprint {height = h, width = w} = stepUntilStableOrN (h * w * 4) (fromJust . makeFactory $ b) emptyResources

blueprintOfOps :: Gen Blueprint
blueprintOfOps = do
  width <- (6 +) . abs <$> arbitrary
  height <- (6 +) . abs <$> arbitrary
  operators <- (arbitrary `suchThat` (\l -> length l > 2) :: Gen [Operator]) -- not sure 10 is the right number. Maybe a function of height and width?
  let machines = Op <$> operators
  fixed <- elements [Set.empty :: Set Point]
  let base = blankBlueprint width height
  -- subtracting from and adding to the height allows us to guarantee that all inputs/outputs are reachable
  let placeInBounds = placeMachineAt . (+>> Point 0 2) . wrapInBounds width (height - 4)
  points <- (arbitrary `suchThat` (\l -> length l == length machines) :: Gen [Point])
  let addCMDs = zipWith ($) (Prelude.map placeInBounds points) machines
  let withMachines = Prelude.foldr (\m b -> m b) base addCMDs
  return withMachines {fixedPoints = fixed}

isOp :: Machine -> Bool
isOp m = case m of
  Op _ -> True
  _ -> False

openOutputs :: Blueprint -> [Point]
openOutputs b@Blueprint {grid = g} = if isJust $ makeFactory b then Prelude.filter (\p -> isMakingResource p && (not . connectsToWire) p) outputList else []
  where
    r = getBoardResources b
    ops = Map.filter isOp g
    outputList = concat . Map.elems $ Map.mapWithKey (\k a -> Prelude.map (+>> k) ((opOutputs . op) a)) ops
    isMakingResource p = isJust $ Map.lookup p (vertical r)
    connectsToWire p = case Map.lookup (p +>> Point 0 (-1)) g of
      Just w@(Wire _) -> connectsToNorth $ direction w
      _ -> False

testOpenOutputs :: Test
testOpenOutputs =
  TestList
    [ openOutputs exB0 ~?= [],
      openOutputs exB1 ~?= [],
      openOutputs exB2 ~?= [Point {pointX = 1, pointY = 1}],
      openOutputs exB3 ~?= [],
      openOutputs exB4 ~?= [Point {pointX = 1, pointY = 1}]
    ]

openInputs :: Blueprint -> [Point]
openInputs b@Blueprint {grid = g} = if isJust $ makeFactory b then Prelude.filter (not . isGettingResource) inputList else []
  where
    r = getBoardResources b
    ops = Map.filter isOp g
    inputList = concat . Map.elems $ Map.mapWithKey (\k a -> Prelude.map (+>> k) a) (Map.map (opInputs . op) ops)
    isGettingResource p = isJust $ Map.lookup (p +>> Point 0 1) (vertical r)

testOpenInputs :: Test
testOpenInputs =
  TestList
    [ openInputs exB0 ~?= [Point {pointX = 0, pointY = 2}, Point {pointX = 2, pointY = 2}],
      openInputs exB1 ~?= [Point {pointX = 2, pointY = 2}],
      openInputs exB2 ~?= [],
      openInputs exB3 ~?= [],
      openInputs exB4 ~?= []
    ]

-- Simple example blueprints to test openInputs and openOutputs
exB0 :: Blueprint
exB0 =
  Blueprint
    { fixedPoints = Set.fromList [],
      grid =
        Map.fromList
          [ (Point {pointX = 0, pointY = 2}, Occupied),
            (Point {pointX = 1, pointY = 1}, Occupied),
            (Point {pointX = 1, pointY = 2}, Op Multiply),
            (Point {pointX = 2, pointY = 2}, Occupied)
          ],
      minimumSinksToSatisfy = 0,
      width = 4,
      height = 10
    }

exB1 :: Blueprint
exB1 = placeMachineAt (Point 0 3) (Source 5) exB0

exB2 :: Blueprint
exB2 = placeMachineAt (Point 2 3) (Source 6) exB1

exB3 :: Blueprint
exB3 = placeMachineAt (Point 1 0) (Wire Vertical) exB2

exB4 :: Blueprint
exB4 = placeMachineAt (Point 1 0) (Wire Horizontal) exB2

placeSinksAtOpenOutputs :: Blueprint -> Blueprint
placeSinksAtOpenOutputs b = Prelude.foldr ($) b placeSinkCmds
  where
    r = getBoardResources b
    placeSinkCmds = Prelude.map (\p -> placeMachineAt (p +>> Point 0 (-1)) (Sink . fromJust . fromJust . Map.lookup p $ vertical r)) (openOutputs b)

addSourceAbove :: Point -> Gen Blueprint -> Gen Blueprint
addSourceAbove p b = do placeMachineAt (p +>> Point 0 1) <$> (Source . (`mod` 64) <$> arbitrary) <*> b

connectOutputToOneOf :: Blueprint -> Point -> [Point] -> Gen Blueprint
connectOutputToOneOf b pSource ys = if Prelude.null reachable then pure b else Prelude.foldr placeWire b <$> wiresToAdd
  where
    p = pSource +>> Point 0 (-1)
    bfsTree = bfsFrom (blueprintToGraph b) (pointToInt b p)
    reachable = Prelude.filter (\p' -> pointToInt b p' `GraphUtils.elem` bfsTree) $ (+>> Point 0 1) <$> ys
    shortestPathInBlueprint q = intToPoint b <$> fromJust ((pointToInt b p ~> pointToInt b q) bfsTree)
    connection = (\t -> (p +>> Point 0 1) : shortestPathInBlueprint t ++ [t ->> Point 0 1]) <$> elements reachable
    wiresToAdd = Prelude.map tripleToWireType . toTriples <$> connection

placeWire (p, w) b@Blueprint {grid = g} = placeMachineAt p wire b
  where
    wire = case Map.lookup p g of
      Nothing -> Wire w
      Just _ -> Wire Overlap

toTriples :: [a] -> [(a, a, a)]
toTriples [x1, x2, x3] = [(x1, x2, x3)]
toTriples (x1 : x2 : x3 : xs) = (x1, x2, x3) : toTriples (x2 : x3 : xs)
toTriples _ = error "List with fewer than 3 elements passed to `toTriples`"

--                   From   Thru   To
tripleToWireType :: (Point, Point, Point) -> (Point, WireType)
tripleToWireType (p1, p2, p3) =
  ( p2,
    case p2 ->> p1 of
      Point 0 1 -> case p2 ->> p3 of
        Point 0 1 -> error $ "Wire cannot enter and exit same point: " ++ show (p1, p2, p3)
        Point 0 (-1) -> Vertical
        Point 1 0 -> SW
        Point (-1) 0 -> SE
        _ -> error $ "Non-adjacent wire triple: " ++ show (p1, p2, p3)
      Point 0 (-1) -> case p2 ->> p3 of
        Point 0 1 -> Vertical
        Point 0 (-1) -> error $ "Wire cannot enter and exit same point: " ++ show (p1, p2, p3)
        Point 1 0 -> NW
        Point (-1) 0 -> NE
        _ -> error $ "Non-adjacent wire triple: " ++ show (p1, p2, p3)
      Point 1 0 -> case p2 ->> p3 of
        Point 0 1 -> SW
        Point 0 (-1) -> NW
        Point 1 0 -> error $ "Wire cannot enter and exit same point: " ++ show (p1, p2, p3)
        Point (-1) 0 -> Horizontal
        _ -> error $ "Non-adjacent wire triple: " ++ show (p1, p2, p3)
      Point (-1) 0 -> case p2 ->> p3 of
        Point 0 1 -> SE
        Point 0 (-1) -> NE
        Point 1 0 -> Horizontal
        Point (-1) 0 -> error $ "Wire cannot enter and exit same point: " ++ show (p1, p2, p3)
        _ -> error $ "Non-adjacent wire triple: " ++ show (p1, p2, p3)
      _ -> error $ "Non-adjacent wire triple: " ++ show (p1, p2, p3)
  )

connectBlueprint :: Gen Blueprint -> Gen Blueprint
connectBlueprint b = do
  b' <- b
  case openOutputs b' of
    xs@(_ : _) -> case openInputs b' of
      ys@(_ : _) ->
        frequency
          [ (1, placeSinksAtOpenOutputs <$> b),
            (10, connectBlueprint . join $ connectOutputToOneOf <$> b <*> elements xs <*> pure ys)
          ]
      [] -> placeSinksAtOpenOutputs <$> b
    [] -> case openInputs b' of
      p : _ ->
        frequency
          [ (1, placeSinksAtOpenOutputs <$> b),
            (10, connectBlueprint (addSourceAbove p b))
          ]
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
    g' = Map.filterWithKey (\p _ -> p `Prelude.elem` ps) g

getNumSinks :: Blueprint -> Int
getNumSinks Blueprint {grid = g} = Map.foldr (+) 0 $ Map.map sinkToInt g
  where
    sinkToInt p = case p of
      Sink _ -> 1
      _ -> 0

solvableBlueprintGen :: Gen Blueprint
solvableBlueprintGen = do
  b <- blueprintOfOps
  b' <- fixSinksAndSources <$> connectBlueprint (return b)
  -- For some reason, the previous lines differ from doing: connectBlueprint blueprintOfOps
  n <- choose (0, getNumSinks b')
  return $ b' {minimumSinksToSatisfy = n}

instance Arbitrary Blueprint where
  arbitrary = do
    width <- abs <$> arbitrary
    height <- abs <$> arbitrary
    machines <- arbitrary :: Gen [Machine]
    fixed <- Set.fromList <$> (arbitrary :: Gen [Point])
    let base = blankBlueprint width height
    withMachines <- liftM3 Prelude.foldr (placeMachineAt <$> arbitrary) (return base) (return machines)
    return withMachines {fixedPoints = fixed}

-- Utility functions for asking

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
             Just m -> Geometry.negate d `Prelude.elem` allOccupied m
         )

prop_placeDidPlace :: Machine -> Gen Property
prop_placeDidPlace m = do
  b <- blueprintWithArea
  p <- elements $ allPoints b
  let b' = placeMachineAt p m b
  return $ b /= b' ==> Just m == getMachineAt p b'

-- NOTE: got a failure once, then got this function passing over separate runs
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