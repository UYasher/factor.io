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
          ( Set.fromList
              <$> Map.fromList
                ( [ (0, [1, 2]),
                    (1, [0, 3]),
                    (2, [0, 3]),
                    (3, [1, 2])
                  ] ::
                    [(Int, [Int])]
                )
          ),
      blueprintToGraph tb1
        ~?= Graph
          ( Set.fromList
              <$> Map.fromList
                ( [ (0, [1, 2]),
                    (1, [3]),
                    (2, [0, 3]),
                    (3, [1, 2])
                  ] ::
                    [(Int, [Int])]
                )
          ),
      blueprintToGraph tb2
        ~?= Graph
          ( Set.fromList
              <$> Map.fromList
                ( [ (0, [1, 2]),
                    (1, [0, 3]),
                    (2, [3]),
                    (3, [1, 2])
                  ] ::
                    [(Int, [Int])]
                )
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

prop_pointToIntIsInvertible :: Blueprint -> Bool
prop_pointToIntIsInvertible b = all (\p -> (intToPoint b . pointToInt b) p == p) (allPoints b)

prop_transposeIsInvolution :: Graph -> Bool
prop_transposeIsInvolution g = (transpose . transpose) g == g

prop_dfsAfterDfsIsStable :: Graph -> Int -> Property
prop_dfsAfterDfsIsStable g x = (x `GraphUtils.elem` g) ==> g' == dfsFrom (transpose g') x
  where
    g' = dfsFrom g x

prop_bfsAfterBfsIsStable :: Graph -> Int -> Property
prop_bfsAfterBfsIsStable g x = (x `GraphUtils.elem` g) ==> g' == bfsFrom (transpose g') x
  where
    g' = bfsFrom g x

prop_dfsProducesSubgraph :: Graph -> Int -> Property
prop_dfsProducesSubgraph g x = (x `GraphUtils.elem` g) ==> g' `isSubgraph` g
  where
    g' = transpose $ dfsFrom g x

prop_bfsProducesSubgraph :: Graph -> Int -> Property
prop_bfsProducesSubgraph g x = (x `GraphUtils.elem` g) ==> g' `isSubgraph` g
  where
    g' = transpose $ bfsFrom g x

prop_bfsElemsEqualDfsElems :: Graph -> Int -> Property
prop_bfsElemsEqualDfsElems g x = (x `GraphUtils.elem` g) ==> GraphUtils.elems b == GraphUtils.elems d
  where
    b = bfsFrom g x
    d = dfsFrom g x

-- Need to add arbitrary instances for graphs