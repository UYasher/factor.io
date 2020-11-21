module GeometryTests where


import Test.HUnit (Assertion, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck
import Geometry

instance Arbitrary Orientation
    where
        arbitrary = elements [North, East, South, West]
        shrink = []

instance Arbitrary Point
    where
        arbitrary = Point <$> arbitrary <*> arbitrary
        shrink Point {pointX=pointX, poinyY=pointY} = do
            x <- shrink pointX
            y <- shrink pointY
            return Point x y

geometryTests :: IO ()
geometryTests = undefined -- do runTestTT (TestList [])

-- prop_rotationsCommute :: Orientation -> Orientation -> Bool
-- prop_rotationsCommute r1 r2 = rotateOrientation r1 r2 == rotateOrientation r2 r1

-- prop_northRotationIsIdentity :: Orientation -> Bool
-- prop_northRotationIsIdentity o = rotateOrientation o North == o

prop_repeatingRotationFourTimesIsIdentity :: Orientation -> Bool
prop_repeatingRotationFourTimesIsIdentity =
    North == foldr (.) (replicate 4 rotateOrientation) id

prop_rotateThenUnrotateIsIdentity :: Orientation -> Bool
prop_rotateThenUnrotateIsIdentity o = rotateOrientation CCW . rotateOrientation CW o == o