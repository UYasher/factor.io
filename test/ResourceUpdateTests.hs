module ResourceUpdateTests where

import Blueprint
import Data.Maybe
import Geometry
import GeometryTests
import ResourceUpdate
import State
import Test.HUnit (Assertion, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck

resourceUpdateTests :: IO ()
resourceUpdateTests = do
  putStrLn "Running ResourceUpdateTests.hs..."
  aux
  putStrLn ""
  where
    aux = do
      quickCheck prop_getHorizEmpty
      quickCheck prop_getVertEmpty
      quickCheck prop_getHorizIsId
      quickCheck prop_getVertIsId
      quickCheck prop_setHorizGetHoriz
      quickCheck prop_setVertGetVert
      quickCheck prop_setHorizGetVert
      quickCheck prop_setVertGetHoriz

instance Arbitrary Resources where
  arbitrary = Resources <$> arbitrary <*> arbitrary

prop_getHorizEmpty :: Point -> Bool
prop_getHorizEmpty p = isNothing $ evalState (getHoriz p) emptyResources

prop_getVertEmpty :: Point -> Bool
prop_getVertEmpty p = isNothing $ evalState (getVert p) emptyResources

prop_getHorizIsId :: Point -> Resources -> Bool
prop_getHorizIsId p r = execState (getHoriz p) r == r

prop_getVertIsId :: Point -> Resources -> Bool
prop_getVertIsId p r = execState (getVert p) r == r

prop_setHorizGetHoriz :: Point -> Point -> Maybe Int -> Bool
prop_setHorizGetHoriz a b mi =
  let v = evalState (setHoriz a mi >> getHoriz b) emptyResources
   in if a == b then v == mi else isNothing v

prop_setVertGetVert :: Point -> Point -> Maybe Int -> Bool
prop_setVertGetVert a b mi =
  let v = evalState (setVert a mi >> getVert b) emptyResources
   in if a == b then v == mi else isNothing v

prop_setHorizGetVert :: Point -> Point -> Maybe Int -> Bool
prop_setHorizGetVert a b mi =
  isNothing $ evalState (setHoriz a mi >> getVert b) emptyResources

prop_setVertGetHoriz :: Point -> Point -> Maybe Int -> Bool
prop_setVertGetHoriz a b mi =
  isNothing $ evalState (setVert a mi >> getHoriz b) emptyResources