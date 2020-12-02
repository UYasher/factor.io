module FactoryTests where

import Blueprint
import Factory
import Geometry
import Machine
import Operator
import OperatorTests
import ResourceUpdate
import State
import Test.HUnit (Assertion, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck
import qualified Data.Set as Set
import BlueprintParsingTests
import BlueprintParsing
import Data.Maybe

factoryTests :: IO ()
factoryTests = do
  putStrLn "Running FactoryTests.hs..."
  aux
  putStrLn ""
  where
    aux = do
      runTestTT testBlankFactory
      runTestTT testSourceFactory
      quickCheck prop_stepBinaryOperator
      quickCheck prop_stepUnitaryOperator
      runTestTT testGetWireSnakeHeads

foldMachinesInto :: Foldable t => Blueprint -> t (Point, Machine) -> Blueprint
foldMachinesInto = foldr $ uncurry placeMachineAt

testBlankFactory :: Test
testBlankFactory =
  (step <$> makeFactory (blankBlueprint 8 8) <*> Just emptyResources)
    ~?= Just emptyResources

testSourceFactory :: Test
testSourceFactory =
  let machines = [(Point 2 1, Source 5), (Point 5 0, Source 18)]
      blueprint = foldMachinesInto (blankBlueprint 8 8) machines
      factory = makeFactory blueprint
      stepped = step <$> factory <*> Just emptyResources
      stateToRun = do
        a <- getVert $ Point 2 1
        b <- getVert $ Point 5 0
        return (a, b)
      actualResult = evalState stateToRun <$> stepped
      expectedResult = Just (Just 5, Just 18)
   in actualResult ~?= expectedResult

prop_stepBinaryOperator :: Operator -> BoundedInt -> BoundedInt -> Property
prop_stepBinaryOperator op (BI x) (BI y) =
  let machines =
        [ (Point 0 2, Source x),
          (Point 2 2, Source y),
          (Point 1 1, Op op)
        ]
      blueprint = foldMachinesInto (blankBlueprint 4 4) machines
      factory = makeFactory blueprint
      stepped1 = step <$> factory <*> Just emptyResources
      stepped2 = step <$> factory <*> stepped1
      stateToRun = getVert $ Point 1 0
      actualResult = evalState stateToRun <$> stepped2
      -- head is guaranteed to be safe, since we have a binary operator, which
      -- always returns one value
      expectedResult = Just $ Just $ head $ f op [x, y]
   in (op /= factoringOperator && op /= duplicationOperator) ==> actualResult == expectedResult

prop_stepUnitaryOperator :: Operator -> BoundedInt -> Property
prop_stepUnitaryOperator op (BI x) =
  let machines = [(Point 1 3, Source x), (Point 1 1, Op op)]
      blueprint = foldMachinesInto (blankBlueprint 4 4) machines
      factory = makeFactory blueprint
      stepped1 = step <$> factory <*> Just emptyResources
      stepped2 = step <$> factory <*> stepped1
      stateToRun = do
        a <- getVert $ Point 0 1
        b <- getVert $ Point 2 1
        return (a, b)
      actualResult = evalState stateToRun <$> stepped2
      intermediate = Just <$> f op [x]
      -- subscript is guaranteed to be safe, since we have a unitary value,
      -- which always returns two values
      expectedResult = Just (head intermediate, intermediate !! 1)
   in (op == factoringOperator || op == duplicationOperator) ==> actualResult == expectedResult


testGetWireSnakeHeads :: Test
testGetWireSnakeHeads = TestList [
    Set.fromList (getWireSnakeHeads g1) ~?= Set.fromList [Point 2 2, Point 4 2, Point 3 5, Point 2 7, Point 4 7],
    Set.fromList (getWireSnakeHeads g2) ~?= Set.fromList [Point 1 0],
    Set.fromList (getWireSnakeHeads g3) ~?= Set.fromList [Point 0 4]
  ]
  where
    g1 = grid . fromJust $ stringToBlueprint blueprint1
    g2 = grid . fromJust $ stringToBlueprint blueprint2
    g3 = grid . fromJust $ stringToBlueprint blueprint3
