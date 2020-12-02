module BlueprintParsingTests where

import BlueprintParsing
import Test.HUnit (Assertion, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck
import ResourcePrinting
import Data.Maybe

-- can we tell quickcheck that certain tests should be run with ints in a specific range?

blueprintParsingTests :: IO ()
blueprintParsingTests = do
  putStrLn "Running BlueprintParsingTests.hs..."
  aux
  putStrLn ""
  where
    aux = do
      runTestTT $ check blueprint1
      runTestTT $ check blueprint2
      putArr $ fromJust (stepNToStrings 35 <$> stringToBlueprint blueprint1)

putArr :: [String] -> IO ()
putArr = mapM_ putStr

check :: String -> Test
check s = blueprintToString <$> stringToBlueprint s ~?= Just s

blueprint1 :: String
blueprint1 = unlines [
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
blueprint2 = unlines [
  "VOV V .",
  "V+V@",
  " V @",
  " | @"
  ]

blueprint3 :: String
blueprint3 = unlines [
  "S . . . . .",
  "4  {___}   @",
  "|  |   |   @",
  "|  |   |   @",
  "|  [___.___@",
  "|      |   @",
  "[______]   @"
  ]