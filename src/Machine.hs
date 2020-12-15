module Machine where

import Control.Monad
import Data.Char
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Geometry
import Operator
import ResourceUpdate
import State
import Wire

-- | Represents the different forms a `Machine` can take
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
  deriving (Eq, Ord)

-- | Returns all of the points in local coordinate space that the given Machine occupies
allOccupied :: Machine -> [Point]
allOccupied (Op op) = [zero] ++ opInputs op ++ opOutputs op
allOccupied _ = [zero]

-- | Returns `Just` the string to use as the field for this `Machine` in a `Budget`,
-- or `Nothing`
machineBudgetField :: Machine -> Maybe String
machineBudgetField (Source _) = Just "source"
machineBudgetField (Sink _) = Just "sink"
machineBudgetField (Op op) = Just $ opBudgetField op
machineBudgetField Occupied = Nothing
machineBudgetField (Wire _) = Just "wire"

instance Show Machine where
  show (Op op) = "Op " ++ [opToChar op]
  show (Source v) = "Source " ++ show v
  show (Sink v) = "Sink " ++ show v
  show Occupied = "Occupied"
  show (Wire dir) = "Wire " ++ show dir

-- | Returns the graphical character to be displayed to represent the given machine.
-- For internal debugging use only.
machineToChar :: Machine -> Char
machineToChar (Op op) = opToChar op
machineToChar (Source v) = valueToChar v
machineToChar (Sink v) = valueToChar v
machineToChar Occupied = 'V'
machineToChar (Wire dir) = wireToChar dir

-- | Returns the graphical character to be displayed to represent the given value.
-- For internal debugging use only.
valueToChar :: Int -> Char
valueToChar x | 0 <= x && x <= 9 = intToDigit x
valueToChar x | 10 <= x && x <= 35 = chr $ ord 'A' + (x - 10)
valueToChar x | 36 <= x && x <= 61 = chr $ ord 'a' + (x - 36)
valueToChar 62 = '!'
valueToChar 63 = '?'

-- | Inverse of `valueToChar`.
-- For internal debugging use only.
charToValue :: Char -> Maybe Int
charToValue c | isDigit c = Just $ digitToInt c
charToValue c | isUpper c = Just $ ord c - 65 + 10
charToValue c | isLower c = Just $ ord c - 97 + 36
charToValue '!' = Just 62
charToValue '?' = Just 63
