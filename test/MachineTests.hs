module MachineTests where

import Machine
import OperatorTests
import Test.HUnit (Assertion, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck
import WireTests

-- can we tell quickcheck that certain tests should be run with ints in a specific range?

machineTests :: IO ()
machineTests = do
  putStrLn "Running MachineTests.hs..."
  aux
  putStrLn ""
  where
    aux = do
      runTestTT testValueToCharCharToValue

instance Arbitrary Machine where
  arbitrary =
    oneof
      [ Op <$> arbitrary,
        Source . int <$> (arbitrary :: Gen BoundedInt),
        Sink . int <$> (arbitrary :: Gen BoundedInt),
        return Occupied,
        Wire <$> arbitrary
      ]

testValueToCharCharToValue :: Test
testValueToCharCharToValue = TestList [charToValue (valueToChar x) ~?= Just x | x <- [0 .. 63]]