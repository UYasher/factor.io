{-# LANGUAGE LambdaCase #-}

module BudgetTests where

import Blueprint
import BlueprintTests
import Budget
import qualified Data.Map as Map
import Machine
import Operator
import OperatorTests
import Test.HUnit (Assertion, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck
import Wire

budgetTests :: IO ()
budgetTests = do
  putStrLn "Running BudgetTests.hs..."
  aux
  putStrLn ""
  where
    aux = do
      quickCheck prop_makeBudgetForSinks
      quickCheck prop_makeBudgetForSources
      quickCheck prop_makeBudgetForWires
      quickCheck prop_makeBudgetForOperators

prop_makeBudgetForSinks :: Blueprint -> Bool
prop_makeBudgetForSinks b@Blueprint {grid = g} =
  let expected = Just $ sum $ map (\case Sink _ -> 1; _ -> 0) $ Map.elems g
      field = machineBudgetField $ Sink 0
      actual = (\f -> get f $ makeBudgetFrom b) <$> field
   in expected == actual

prop_makeBudgetForSources :: Blueprint -> Bool
prop_makeBudgetForSources b@Blueprint {grid = g} =
  let expected = Just $ sum $ map (\case Source _ -> 1; _ -> 0) $ Map.elems g
      field = machineBudgetField $ Source 0
      actual = (\f -> get f $ makeBudgetFrom b) <$> field
   in expected == actual

prop_makeBudgetForWires :: Blueprint -> Bool
prop_makeBudgetForWires b@Blueprint {grid = g} =
  let expected = Just $ sum $ map aux $ Map.elems g
      field = machineBudgetField $ Wire Vertical
      actual = (\f -> get f $ makeBudgetFrom b) <$> field
   in expected == actual
  where
    aux m = case m of
      Wire _ -> 1
      _ -> 0

prop_makeBudgetForOperators :: Operator -> Blueprint -> Bool
prop_makeBudgetForOperators o b@Blueprint {grid = g} =
  let expected = sum $ map aux $ Map.elems g
      field = opBudgetField o
      actual = get field $ makeBudgetFrom b
   in expected == actual
  where
    aux m = if m == Op o then 1 else 0