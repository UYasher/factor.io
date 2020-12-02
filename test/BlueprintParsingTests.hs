module BlueprintParsingTests where

import Blueprint
import BlueprintParsing
import Data.Maybe
import Factory
import ResourcePrinting
import ResourceUpdate
import Test.HUnit (Assertion, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck

-- can we tell quickcheck that certain tests should be run with ints in a specific range?

blueprintParsingTests :: IO ()
blueprintParsingTests = do
  putStrLn "Running BlueprintParsingTests.hs..."
  aux
  putStrLn ""
  where
    aux = do
      runTestTT $ testParseUnparse blueprint1
      runTestTT $ testParseUnparse blueprint2
      runTestTT $ testIsBadFactory badBlueprint4
      runTestTT $ testIsBadFactory badBlueprint5
      runTestTT $ testIsBadFactory badBlueprint6
      runTestTT $ testBecomesSatisfied True $ fromJust $ stringToBlueprint blueprint1
      runTestTT $ testBecomesSatisfied False $ fromJust $ stringToBlueprint unsatisfiableBlueprint7
      runTestTT $ testBecomesSatisfied True $ fromJust $ stringToBlueprint easyToSatisfyBlueprint8

-- uncomment to see the result of running `blueprint1` for `35` steps
-- putArr $ fromJust (stepNToStrings 35 <$> stringToBlueprint blueprint1)

putArr :: [String] -> IO ()
putArr = mapM_ putStr

testParseUnparse :: String -> Test
testParseUnparse s = blueprintToString <$> stringToBlueprint s ~?= Just s

testIsBadFactory :: String -> Test
testIsBadFactory = (~?= True) . isNothing . makeFactory . fromJust . stringToBlueprint

testBecomesSatisfied :: Bool -> Blueprint -> Test
testBecomesSatisfied bool b = (~?= Just True) $ stepper <$> makeFactory b
  where
    stepper = \f -> bool == isSatisfied b (stepUntilStableOrN 1000 f emptyResources)

blueprint1 :: String
blueprint1 =
  unlines
    [ "2",
      "SS VOV V . V VOV . . GG",
      "  3 7  @",
      "  V+V  @",
      "   V   @",
      "   |   @",
      "   V   @",
      "  VFV  @",
      " {] [} @",
      " |   | @",
      " 2   5 @"
    ]

blueprint2 :: String
blueprint2 =
  unlines
    [ "0",
      "VOV V .",
      "V+V@",
      " V @",
      " | @"
    ]

blueprint3 :: String
blueprint3 =
  unlines
    [ "0",
      "S . . . . .",
      "4  {___}   @",
      "|  |   |   @",
      "|  |   |   @",
      "|  [___.___@",
      "|      |   @",
      "[______]   @"
    ]

badBlueprint4 :: String
badBlueprint4 =
  unlines
    [ "0",
      "SS .",
      "4   5 @",
      "[___] @"
    ]

badBlueprint5 :: String
badBlueprint5 =
  unlines
    [ "0",
      "VOV V . VOV . V . .",
      "V+V    @",
      " V     @",
      " |     @",
      " | V-V @",
      " |  V  @",
      " |  |  @",
      " [__]"
    ]

badBlueprint6 :: String
badBlueprint6 =
  unlines
    [ "0",
      "S . VOV V",
      "2      @",
      "|      @",
      "|  V/V @",
      "|   V  @",
      "[___]  @"
    ]

unsatisfiableBlueprint7 :: String
unsatisfiableBlueprint7 =
  unlines
    [ "4",
      "SS VOV V . V VOV . . GG",
      "  3 7  @",
      "  V+V  @",
      "   V   @",
      "   |   @",
      "   V   @",
      "  VFV  @",
      " {] [} @",
      " |   | @",
      " 2   5 @"
    ]

easyToSatisfyBlueprint8 :: String
easyToSatisfyBlueprint8 =
  unlines
    [ "1",
      "SS VOV V . V VOV . . G . G",
      "  3 7  @",
      "  V+V  @",
      "   V   @",
      "   |   @",
      "   V   @",
      "  VFV  @",
      " {] [} @",
      " |   | @",
      " 2   | @",
      "     | @",
      "     5 @"
    ]