module MachineTests where

import Test.HUnit (Assertion, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck
import Machines



prop_machinesRotatedFourTimesIsIdentity :: Machine -> Orientation -> Bool
prop_machinesRotatedFourTimesIsIdentity m o = 
    m == foldr (.) (replicate 4 (`rotateMachine` o)) id $ m

-- unit tests about specific machine types' behavior