module MachineTests where

import Machine
import Test.HUnit (Assertion, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck
import WireTests
import OperatorTests

-- can we tell quickcheck that certain tests should be run with ints in a specific range?

machineTests :: IO ()
machineTests = do
  putStrLn "Running MachineTests.hs..."
  aux
  putStrLn ""
  where
    aux = do
      putStrLn "No tests yet"

instance Arbitrary Machine where
  arbitrary =
    oneof
      [ Op <$> arbitrary,
        Source . int <$> (arbitrary :: Gen BoundedInt),
        Sink . int <$> (arbitrary :: Gen BoundedInt),
        return Occupied,
        Wire <$> arbitrary
      ]