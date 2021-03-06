module GeometryTests where

import Geometry
import Test.HUnit (Assertion, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck

instance Arbitrary Point where
  arbitrary = Point <$> arbitrary <*> arbitrary
  shrink Point {pointX = pointX, pointY = pointY} = do
    x <- shrink pointX
    y <- shrink pointY
    return $ Point x y

wrapInBounds :: Int -> Int -> Point -> Point
wrapInBounds w h (Point x y) = Point (x `safemod` w) (y `safemod` h)
  where
    m `safemod` d = if d == 0 then 0 else m `mod` d

boundedPoints :: Int -> Int -> Gen [Point]
boundedPoints w h = do
  points <- (arbitrary :: Gen [Point])
  return $ wrapInBounds w h <$> points

geometryTests :: IO ()
geometryTests = do
  putStrLn "Running GeometryTests.hs..."
  aux
  putStrLn ""
  where
    aux = do
      quickCheck prop_pointAddition
      quickCheck prop_pointSubtraction
      quickCheck prop_pointSubtractionIsAddingNegation

prop_pointAddition :: Point -> Point -> Bool
prop_pointAddition p1@(Point x1 y1) p2@(Point x2 y2) =
  (p1 +>> p2) == Point (x1 + x2) (y1 + y2)

prop_pointSubtraction :: Point -> Point -> Bool
prop_pointSubtraction p1@(Point x1 y1) p2@(Point x2 y2) =
  (p1 ->> p2) == Point (x1 - x2) (y1 - y2)

prop_pointSubtractionIsAddingNegation :: Point -> Point -> Bool
prop_pointSubtractionIsAddingNegation p1 p2 = p1 ->> p2 == p1 +>> Geometry.negate p2