module WireTests where

import Test.HUnit (Assertion, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck
import Wire

wireTests :: IO ()
wireTests = do
  putStrLn "Running WireTests.hs..."
  aux
  putStrLn ""
  where
    aux = do
      putStrLn "no tests yet"

instance Arbitrary WireType where
  arbitrary = elements [Vertical, Horizontal, NE, SE, SW, NW, Overlap]