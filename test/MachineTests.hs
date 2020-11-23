module MachineTests where

import Machines
import Test.HUnit (Assertion, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck

prop_machinesRotatedFourTimesIsIdentity :: Machine a -> Orientation -> Bool
prop_machinesRotatedFourTimesIsIdentity m o =
  m == foldr (.) (replicate 4 (`rotateMachine` o)) id $ m

-- unit tests about specific machine types' behavior