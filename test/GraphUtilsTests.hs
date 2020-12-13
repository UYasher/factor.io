module GraphUtilsTests where

import Blueprint
import Control.Monad
import Data.Map as Map
import Data.Set as Set
import Geometry
import GeometryTests
import GraphUtils
import Machine
import MachineTests ()
import Test.HUnit
import Test.QuickCheck
import Wire

instance Arbitrary Blueprint where
  arbitrary = do
    width <- abs <$> arbitrary
    height <- abs <$> arbitrary
    machines <- arbitrary :: Gen [Machine]
    fixed <- Set.fromList <$> (arbitrary :: Gen [Point])
    let base = blankBlueprint width height
    withMachines <- liftM3 Prelude.foldr (placeMachineAt <$> arbitrary) (return base) (return machines)
    return withMachines {fixedPoints = fixed}

testBlueprintToGraph :: Test
testBlueprintToGraph =
  TestList
    [ blueprintToGraph tb0
        ~?= Graph
          ( Map.fromList
              [ (0, [1, 2]),
                (1, [0, 3]),
                (2, [0, 3]),
                (3, [1, 2])
              ]
          ),
      blueprintToGraph tb1
        ~?= Graph
          ( Map.fromList
              [ (0, [1, 2]),
                (1, [3]),
                (2, [0, 3]),
                (3, [1, 2])
              ]
          ),
      blueprintToGraph tb2
        ~?= Graph
          ( Map.fromList
              [ (0, [1, 2]),
                (1, [0, 3]),
                (2, [3]),
                (3, [1, 2])
              ]
          )
    ]

tb0 :: Blueprint
tb0 =
  Blueprint
    { fixedPoints = Set.fromList [],
      grid = Map.empty,
      minimumSinksToSatisfy = 0,
      width = 2,
      height = 2
    }

tb1 :: Blueprint
tb1 = placeMachineAt (Point 0 0) (Wire Horizontal) tb0

tb2 :: Blueprint
tb2 = placeMachineAt (Point 0 0) (Wire Vertical) tb0

prop_pointIntConversion :: Blueprint -> Bool
prop_pointIntConversion b = all (\p -> (intToPoint b . pointToInt b) p == p) (allPoints b)