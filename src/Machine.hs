module Machine where

import Data.Char
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Geometry
import Wire

-- | Represents the concept of a machine
-- | `Op` represents an Operator that manipulates numbers to produce new numbers
-- | `Source` represents a machine that creates a value
-- | `Sink` represents a machien that accepts a value
-- | `Occupied` represents an input or output port (useful for avoiding overlap in the 2d grid)
-- | `Wire` represents a wire that transfers values between machines
data Machine
  = Op {op :: Operator}
  | Source {value :: Int}
  | Sink {value :: Int}
  | Occupied
  | Wire {direction :: WireType}
  deriving (Eq)

instance Show Machine where
  show = (: []) . machineToChar

-- | Returns the graphical character to be displayed to represent the given machine
machineToChar :: Machine -> Char
machineToChar (Op op) = opToChar op
machineToChar (Source v) = valueToChar v
machineToChar (Sink v) = valueToChar v
machineToChar Occupied = 'V'
machineToChar (Wire dir) = wireToChar dir

-- | Returns the graphical character to be displayed to represent the given value
valueToChar :: Int -> Char
valueToChar x | 0 <= x && x <= 9 = intToDigit x
valueToChar x | 10 <= x && x <= 35 = chr $ ord 'A' + (x - 10)
valueToChar x | 36 <= x && x <= 61 = chr $ ord 'a' + (x - 36)
valueToChar 62 = '!'
valueToChar 63 = '?'

-- | Returns all of the points in local coordinate space that the given Machine occupies
allOccupied :: Machine -> [Point]
allOccupied (Op op) = [zero] ++ inputs op ++ outputs op
allOccupied _ = [zero]

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

-- -- | Represents the internal state accumlated by a machine, after processing input
-- newtype MachineState a = MachineState {runMachineState :: a -> (a, MachineState a)}

-- -- | Holds the state of a machine, as well as how it integrates into the game board
-- data Machine a = Machine
--   { state :: MachineState a,
--     orientation :: Orientation,
--     inputPointOffsets :: [Point],
--     outputPointOffsets :: [Point]
--   }

-- rotateMatchine :: Orientation -> Machine a -> Machine a
-- rotateMatchine = undefined

-- -- Generates a map from input strings to output strings, as specified as the contents of the argument
-- -- Useful for making machines whose behavior is specified in text files
-- loadContents :: String -> Map.Map String String
-- loadContents str =
--   let ls = lines str
--       parsedLines = mapMaybe parse ls
--    in foldr (uncurry Map.insert) Map.empty parsedLines
--   where
--     parse :: String -> Maybe (String, String)
--     parse l = undefined

-- -- let ss = words l
-- -- -- warning: unsafe head
-- --     n = (readMaybe $ head ss) :: Maybe Int
-- --     in n >>= \n' ->
-- --       let inputs = take n' (drop 1 ss)
-- --           outputs = drop (n' + 1) ss
-- --           in (unwords inputs, unwords outputs)