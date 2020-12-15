module GraphUtilsTests where

import Blueprint
import Budget
import Control.Monad
import Data.Map as Map
import Data.Maybe
import Data.Set as Set
import Geometry
import GeometryTests
import GraphUtils
import Machine
import MachineTests ()
import Test.HUnit
import Test.QuickCheck
import Wire

graphUtilsTests :: IO ()
graphUtilsTests = do
  putStrLn "Running GraphUtilsTests.hs..."
  aux
  putStrLn ""
  where
    aux = do
      runTestTT testBlueprintToGraph
      quickCheck prop_pointToIntIsInvertible
      quickCheck prop_transposeIsInvolution
      quickCheck prop_dfsAfterDfsIsStable
      quickCheck prop_bfsAfterBfsIsStable
      quickCheck prop_dfsProducesSubgraph
      quickCheck prop_bfsProducesSubgraph
      quickCheck prop_bfsElemsEqualDfsElems
      quickCheck prop_bfsPathLength

instance Arbitrary Blueprint where
  arbitrary = do
    width <- abs <$> arbitrary
    height <- abs <$> arbitrary
    machines <- arbitrary :: Gen [Machine]
    fixed <- Set.fromList <$> (arbitrary :: Gen [Point])
    let base = blankBlueprint width height
    withMachines <- liftM3 Prelude.foldr (placeMachineAt <$> arbitrary) (return base) (return machines)
    return withMachines {fixedPoints = fixed}

instance Arbitrary Graph where
  arbitrary = do
    numNodes <- abs <$> arbitrary :: Gen Int
    let f _ = do
          h <- (fmap . fmap) (\x -> choose (0, numNodes)) (arbitrary :: Gen [Int])
          let h' = sequence h
          Set.fromList <$> h'
    (Graph <$>) . sequence $ Prelude.foldr (\x m -> Map.insert x (f x) m) Map.empty [0 .. numNodes]

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
      budget = zeroBudget,
      width = 2,
      height = 2
    }

tb1 :: Blueprint
tb1 = placeMachineAt (Point 0 0) (Wire Horizontal) tb0

tb2 :: Blueprint
tb2 = placeMachineAt (Point 0 0) (Wire Vertical) tb0

graphMaintainsAdjacency :: Graph -> Blueprint -> Bool
graphMaintainsAdjacency (Graph m) b =
  all
    ( \k ->
        all
          (isAdjacent b (intToPoint b k) . intToPoint b)
          (Set.toList . fromJust $ Map.lookup k m)
    )
    (Map.keys m)

prop_blueprintToGraphMaintainsAdjacency :: Blueprint -> Bool
prop_blueprintToGraphMaintainsAdjacency b = graphMaintainsAdjacency (blueprintToGraph b) b

prop_bfsOfBlueprintMaintainsAdjacency :: Blueprint -> Int -> Property
prop_bfsOfBlueprintMaintainsAdjacency b x = (x `GraphUtils.elem` g) ==> graphMaintainsAdjacency g' b
  where
    g = blueprintToGraph b
    g' = bfsFrom g x

prop_pathOfBlueprintMaintainsAdjacency :: Blueprint -> Int -> Int -> Property
prop_pathOfBlueprintMaintainsAdjacency b x y =
  (x `GraphUtils.elem` g) && (y `GraphUtils.elem` g') && isJust path
    ==> all (\(v1, v2) -> isAdjacent b (intToPoint b v1) (intToPoint b v2) || v1 == v2) (toDoubles $ fromJust path)
  where
    g = blueprintToGraph b
    g' = bfsFrom g x
    path = (x ~> y) g'
    toDoubles [x1, x2] = [(x1, x2)]
    toDoubles (x1 : x2 : xs) = (x1, x2) : toDoubles (x2 : xs)

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

-- | A test serving as a proxy for the shortest path property of bfs.
-- Note: this property runs very close to the 1000 example limit for quickCheck,
-- you may need to run it a couple of times before passing without timing out.
prop_bfsPathLength :: Graph -> Int -> Int -> Property
prop_bfsPathLength g x y =
  (x `GraphUtils.elem` g) && (y `GraphUtils.elem` g)
    ==> (Set.size . GraphUtils.elems $ bfsFrom (bfsFrom (transpose g) x) y) == (Set.size . GraphUtils.elems $ bfsFrom (bfsFrom g y) x)