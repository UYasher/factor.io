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
  show (Op op) = "Op " ++ [opToChar op]
  show (Source v) = "Source " ++ show v
  show (Sink v) = "Sink " ++ show v
  show Occupied = "Occupied"
  show (Wire dir) = "Wire " ++ show dir

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

-- | Inverse of `valueToChar`
charToValue :: Char -> Maybe Int
charToValue c | isDigit c = Just $ digitToInt c
charToValue c | isUpper c = Just $ ord c - 65 + 10
charToValue c | isLower c = Just $ ord c - 97 + 36
charToValue '!' = Just 62
charToValue '?' = Just 63

-- | Returns all of the points in local coordinate space that the given Machine occupies
allOccupied :: Machine -> [Point]
allOccupied (Op op) = [zero] ++ inputs op ++ outputs op
allOccupied _ = [zero]

-- -- | Tells a machine to perform a state update at the specified point.
-- -- | (The point must be specified because machines calculate space relative to
-- -- | their local origin, which means they don't know where they are globally.)
-- machineToStateUpdate :: Machine -> Point -> State Resources ()
-- machineToStateUpdate (Op op) p = do
--   let globalInputs = map (getVert . (+>> p)) $ inputs op
--   let globalOutputs = map (setVert . (+>> p)) $ outputs op
--   mis <- foldr (liftM2 (:)) (return []) globalInputs
--   case sequence mis of
--     Nothing -> return ()
--     Just l ->
--       let result = f op l
--        in foldr (\(x, y) -> ((x $ Just y) >>)) (return ()) (zip globalOutputs result)
-- machineToStateUpdate (Source v) p = do
--   setVert p $ Just v
-- machineToStateUpdate (Sink _) p = do
--   v <- getVert (p +>> Point 0 1)
--   setVert p v
-- machineToStateUpdate Occupied _ = return ()
-- machineToStateUpdate (Wire dir) p = wireToStateUpdate dir p

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