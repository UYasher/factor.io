module Operator where

import Geometry

-- | `Operator`'s manipulate numbers to produce new numbers. There is a fixed
-- number, with pre-defined behavior
data Operator = Add | Subtract | Multiply | Divide | Modulo | Factor | Duplicate
  deriving (Eq, Show, Read, Ord)

-- | Converts an operator into a function that will manipulate numbers in
-- the "correct" way. (eg, the `Add` function adds its operands)
opFunc :: Operator -> ([Int] -> [Int])
opFunc Add [a, b] = [(a + b) `mod` 64]
opFunc Subtract [a, b] = [(a - b) `mod` 64]
opFunc Multiply [a, b] = [(a * b) `mod` 64]
opFunc Divide [a, b] = [if b == 0 then 0 else a `div` b]
opFunc Modulo [a, b] = [if b == 0 then 0 else a `mod` b]
opFunc Factor [a] = aux [a]
  where
    aux [a] = if a == 0 then [0, 1] else [largestFactorLESquareRoot a, a `div` largestFactorLESquareRoot a]
    largestFactorLESquareRoot a = head $ filter (\x -> x * x <= a && x `divides` a) [63, 62 .. 1]
    a `divides` b = (b `div` a) * a == b
opFunc Duplicate [a] = [a, a]

-- | Converts an operator into an array in local coordiate space of the input ports of the machine, in order
opInputs :: Operator -> [Point]
opInputs Add = [Point (-1) 0, Point 1 0]
opInputs Subtract = [Point (-1) 0, Point 1 0]
opInputs Multiply = [Point (-1) 0, Point 1 0]
opInputs Divide = [Point (-1) 0, Point 1 0]
opInputs Modulo = [Point (-1) 0, Point 1 0]
opInputs Factor = [Point 0 1]
opInputs Duplicate = [Point 0 1]

-- | Converts an operator into an array in local coordiate space of the output ports of the machine, in order
opOutputs :: Operator -> [Point]
opOutputs Add = [Point 0 (-1)]
opOutputs Subtract = [Point 0 (-1)]
opOutputs Multiply = [Point 0 (-1)]
opOutputs Divide = [Point 0 (-1)]
opOutputs Modulo = [Point 0 (-1)]
opOutputs Factor = [Point (-1) 0, Point 1 0]
opOutputs Duplicate = [Point (-1) 0, Point 1 0]

-- | `opToChar` returns the graphical character to be displayed to represent the given operator
opToChar :: Operator -> Char
opToChar Add = '+'
opToChar Subtract = '-'
opToChar Multiply = '*'
opToChar Divide = '/'
opToChar Modulo = '%'
opToChar Factor = 'F'
opToChar Duplicate = 'D'