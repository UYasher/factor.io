module OperatorTests where

import Operator
import Test.HUnit (Assertion, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck
import WireTests

-- can we tell quickcheck that certain tests should be run with ints in a specific range?

operatorTests :: IO ()
operatorTests = do
  putStrLn "Running OperatorTests.hs..."
  aux
  putStrLn ""
  where
    aux = do
      quickCheck $ counterexample "addition" prop_additionOperator
      quickCheck $ counterexample "subtraction" prop_subtractionOperator
      quickCheck $ counterexample "multiplication" prop_multiplicationOperator
      quickCheck $ counterexample "division" prop_divisionOperator
      quickCheck $ counterexample "modulo" prop_moduloOperator
      quickCheck $ counterexample "factoring 1" prop_factoringOperatorGivesTwoNumbers
      quickCheck $ counterexample "factoring 2" prop_factoringOperatorMultipliesToOriginal
      quickCheck $ counterexample "factoring 3" prop_factoringOperatorGivesSmallerNumberFirst
      quickCheck $ counterexample "factoring 4" prop_factoringOperatorGivesClosestToSquareRoot
      quickCheck $ counterexample "duplication" prop_duplicationOperator

-- helper type for making Ints in the range 0..63
newtype BoundedInt = BI Int

int :: BoundedInt -> Int
int (BI x) = x

instance Show BoundedInt where
  show (BI x) = show x

instance Arbitrary BoundedInt where
  arbitrary = BI <$> choose (0, 63)

instance Arbitrary Operator where
  arbitrary =
    elements
      [ Add,
        Subtract,
        Multiply,
        Divide,
        Modulo,
        Factor,
        Duplicate
      ]

prop_additionOperator :: BoundedInt -> BoundedInt -> Bool
prop_additionOperator (BI x) (BI y) =
  opFunc Add [x, y] == [result]
  where
    result = (x + y) `mod` 64

prop_subtractionOperator :: BoundedInt -> BoundedInt -> Bool
prop_subtractionOperator (BI x) (BI y) =
  opFunc Subtract [x, y] == [result]
  where
    result = if x > y then x - y else (64 + x - y) `mod` 64

prop_multiplicationOperator :: BoundedInt -> BoundedInt -> Bool
prop_multiplicationOperator (BI x) (BI y) =
  opFunc Multiply [x, y] == [result]
  where
    result = (x * y) `mod` 64

prop_divisionOperator :: BoundedInt -> BoundedInt -> Bool
prop_divisionOperator (BI x) (BI y) =
  opFunc Divide [x, y] == [result]
  where
    result =
      if y == 0 then 0 else x `div` y

prop_moduloOperator :: BoundedInt -> BoundedInt -> Bool
prop_moduloOperator (BI x) (BI y) =
  opFunc Modulo [x, y] == [result]
  where
    result =
      if y == 0 then 0 else x `mod` y

prop_factoringOperatorGivesTwoNumbers :: BoundedInt -> Bool
prop_factoringOperatorGivesTwoNumbers (BI x) =
  length (opFunc Factor [x]) == 2

prop_factoringOperatorMultipliesToOriginal :: BoundedInt -> Bool
prop_factoringOperatorMultipliesToOriginal (BI x) =
  product (opFunc Factor [x]) == x

prop_factoringOperatorGivesSmallerNumberFirst :: BoundedInt -> Bool
prop_factoringOperatorGivesSmallerNumberFirst (BI x) =
  case opFunc Factor [x] of
    [a, b] -> a <= b
    _ -> error "impossible"

prop_factoringOperatorGivesClosestToSquareRoot :: BoundedInt -> Bool
prop_factoringOperatorGivesClosestToSquareRoot (BI x) =
  case opFunc Factor [x] of
    (a : _) -> helper (a + 1)
    _ -> error "impossible"
  where
    helper a = (a * a > x) || (a * (x `div` a) /= x && helper (a + 1))

prop_duplicationOperator :: BoundedInt -> Bool
prop_duplicationOperator (BI x) =
  opFunc Duplicate [x] == [x, x]