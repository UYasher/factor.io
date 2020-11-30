module Operator where

import Geometry

-- | `Operator`s manipulate numbers to produce new numbers
-- | `f` is the function that defines the operator's behavior
-- | `inputs` is an array in local coordiate space of the input ports of the machine, in order
-- | `outputs` is an array in local coordinate space of the output ports of the machine, in order
-- | `opToChar` returns the graphical character to be displayed to represent the given operator
data Operator = Operator
  { f :: [Int] -> [Int],
    inputs :: [Point],
    outputs :: [Point],
    opToChar :: Char
  }

-- | Note that we have to rely on `opToChar`, since function equality is undecidable
instance Eq Operator where
  a == b = opToChar a == opToChar b

-- | Helper function to create various pre-defined operators
additionOperator :: Operator
additionOperator = Operator aux [Point (-1) 0, Point 1 0] [Point 0 (-1)] '+'
  where
    aux [a, b] = [(a + b) `mod` 64]

subtractionOperator :: Operator
subtractionOperator = Operator aux [Point (-1) 0, Point 1 0] [Point 0 (-1)] '-'
  where
    aux [a, b] = [(a - b) `mod` 64]

multiplicationOperator :: Operator
multiplicationOperator = Operator aux [Point (-1) 0, Point 1 0] [Point 0 (-1)] '*'
  where
    aux [a, b] = [(a * b) `mod` 64]

divisionOperator :: Operator
divisionOperator = Operator aux [Point (-1) 0, Point 1 0] [Point 0 (-1)] '/'
  where
    aux [a, b] = [if b == 0 then 0 else a `div` b]

moduloOperator :: Operator
moduloOperator = Operator aux [Point (-1) 0, Point 1 0] [Point 0 (-1)] '%'
  where
    aux [a, b] = [if b == 0 then 0 else a `mod` b]

factoringOperator :: Operator
factoringOperator = Operator aux [Point 0 1] [Point (-1) 0, Point 1 0] 'F'
  where
    aux [a] = if a == 0 then [0, 1] else [largestFactorLESquareRoot a, a `div` largestFactorLESquareRoot a]
    largestFactorLESquareRoot a = head $ filter (\x -> x * x <= a && x `divides` a) [63, 62 .. 1]
    a `divides` b = (b `div` a) * a == b

duplicationOperator :: Operator
duplicationOperator = Operator aux [Point 0 1] [Point (-1) 0, Point 1 0] 'D'
  where
    aux [a] = [a, a]