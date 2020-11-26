module Machine where

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Geometry
import Text.Read

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

-- | Returns the graphical character to be displayed to represent the given machine
toChar :: Machine -> Char
toChar = undefined

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

-- | Gives the direction a wire runs, which determines what wires it connects to and propagates values to
data WireType = Vertical | Horizontal | NE | SE | SW | NW | Overlap

-- | Returns True iff the given wire can propagate values to the north
connectsToNorth :: WireType -> Bool
connectsToNorth = undefined

connectsToSouth :: WireType -> Bool
connectsToSouth = undefined

connectsToEast :: WireType -> Bool
connectsToEast = undefined

connectsToWest :: WireType -> Bool
connectsToWest = undefined

-- | Helper function to create various pre-defined operators
additionOperator :: Operator
additionOperator = undefined

subtractionOperator :: Operator
subtractionOperator = undefined

multiplicationOperator :: Operator
multiplicationOperator = undefined

divisionOperator :: Operator
divisionOperator = undefined

moduloOperator :: Operator
moduloOperator = undefined

factoringOperator :: Operator
factoringOperator = undefined

duplicatingOperator :: Operator
duplicatingOperator = undefined

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