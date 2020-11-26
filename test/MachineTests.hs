module MachineTests where

import Machine
import Test.HUnit (Assertion, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck

-- can we tell quickcheck that certain tests should be run with ints in a specific range?

machineTests :: IO ()
machineTests = do
  putStrLn "Running GeometryTests.hs..."
  aux
  putStrLn ""
  where
    aux = do
      putStrLn "No implemented behavior"

--   quickCheck prop_additionOperator
--   quickCheck prop_subtractionOperator
--   quickCheck prop_multiplicationOperator
--   quickCheck prop_divisionOperator
--   quickCheck prop_moduloOperator
--   quickCheck prop_factoringOperatorGivesTwoNumbers
--   quickCheck prop_factoringOperatorMultipliesToOriginal
--   quickCheck prop_factoringOperatorGivesSmallerNumberFirst
--   quickCheck prop_factoringOperatorGivesClosestToSquareRoot
--   quickCheck prop_duplicatingOperator

-- helper type for making Ints in the range 0..63
newtype BoundedInt = BI Int

instance Show BoundedInt where
  show (BI x) = show x

instance Arbitrary BoundedInt where
  arbitrary = BI <$> choose (0, 63)

prop_additionOperator :: BoundedInt -> BoundedInt -> Bool
prop_additionOperator (BI x) (BI y) =
  f additionOperator [x, y] == [result]
  where
    result = (x + y) `mod` 64

prop_subtractionOperator :: BoundedInt -> BoundedInt -> Bool
prop_subtractionOperator (BI x) (BI y) =
  f subtractionOperator [x, y] == [result]
  where
    result = if x > y then x - y else (64 + x - y) `mod` 64

prop_multiplicationOperator :: BoundedInt -> BoundedInt -> Bool
prop_multiplicationOperator (BI x) (BI y) =
  f multiplicationOperator [x, y] == [result]
  where
    result = (x * y) `mod` 64

prop_divisionOperator :: BoundedInt -> BoundedInt -> Bool
prop_divisionOperator (BI x) (BI y) =
  f divisionOperator [x, y] == [result]
  where
    result =
      if y == 0 then 0 else x `div` y

prop_moduloOperator :: BoundedInt -> BoundedInt -> Bool
prop_moduloOperator (BI x) (BI y) =
  f moduloOperator [x, y] == [result]
  where
    result =
      if y == 0 then 0 else x `div` y

prop_factoringOperatorGivesTwoNumbers :: BoundedInt -> Bool
prop_factoringOperatorGivesTwoNumbers (BI x) =
  length (f factoringOperator [x]) == 2

prop_factoringOperatorMultipliesToOriginal :: BoundedInt -> Bool
prop_factoringOperatorMultipliesToOriginal (BI x) =
  product (f factoringOperator [x]) == x

prop_factoringOperatorGivesSmallerNumberFirst :: BoundedInt -> Bool
prop_factoringOperatorGivesSmallerNumberFirst (BI x) =
  case f factoringOperator [x] of
    [a, b] -> a <= b
    _ -> error "impossible"

prop_factoringOperatorGivesClosestToSquareRoot :: BoundedInt -> Bool
prop_factoringOperatorGivesClosestToSquareRoot (BI x) =
  case f factoringOperator [x] of
    (a : _) -> helper (a + 1)
    _ -> error "impossible"
  where
    helper a = (a * a > x) || (a * (x `div` a) /= x && helper (a + 1))

prop_duplicatingOperator :: BoundedInt -> Bool
prop_duplicatingOperator (BI x) =
  f factoringOperator [x] == [x, x]