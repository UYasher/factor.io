module WireTests where

import Geometry
import GeometryTests
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
      quickCheck prop_overlapNextPointFrom
      quickCheck prop_nextPointFromInvertsToSelfWhenCompatible

instance Arbitrary WireType where
  arbitrary = elements [Vertical, Horizontal, NE, SE, SW, NW, Overlap]

-- Easy way to generate adjacent points, which Wire uses a lot
data Direction = North | South | East | West deriving (Show)

instance Arbitrary Direction where
  arbitrary = elements [North, South, East, West]

dirToPoint :: Direction -> Point
dirToPoint North = Point 0 1
dirToPoint East = Point 1 0
dirToPoint South = Point 0 (-1)
dirToPoint West = Point (-1) 0

prop_overlapNextPointFrom :: Direction -> Point -> Bool
prop_overlapNextPointFrom dir p =
  nextPointFrom Overlap p (p +>> offset) == p +>> offset +>> offset
  where
    offset = dirToPoint dir

prop_nextPointFromInvertsToSelfWhenCompatible :: Point -> WireType -> Bool
prop_nextPointFromInvertsToSelfWhenCompatible p w =
  and
    ( do
        dir <- compatibleDir w
        let p' = nextPointFrom w p (p ->> dirToPoint dir)
        let p'' = nextPointFrom w p' (p ->> dirToPoint dir)
        return $ p == p''
    )
  where
    compatibleDir :: WireType -> [Direction]
    compatibleDir Overlap = [North, South, East, West]
    compatibleDir Vertical = [North, South]
    compatibleDir Horizontal = [East, West]
    compatibleDir NE = [North, East]
    compatibleDir NW = [North, West]
    compatibleDir SE = [South, East]
    compatibleDir SW = [South, West]
