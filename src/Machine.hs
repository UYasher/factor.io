module Machine where

import Geometry
import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Text.Read


-- | Represents the internal state accumlated by a machine, after processing input
newtype MachineState = MachineState {runMachineState :: [Char] -> (([Char], [Char]), MachineState)}

-- | Holds the state of a machine, as well as how it integrates into the game board
data Machine = Machine
  { state :: MachineState,
    orientation :: Orientation,
    inputPointOffsets :: [Point],
    outputPointOffsets :: [Point]
  }

rotateMatchine :: Orientation -> Machine -> Machine
rotateMatchine = undefined

-- Generates a map from input strings to output strings, as specified as the contents of the argument
-- Useful for making machines whose behavior is specified in text files
loadContents :: String -> Map.Map String String
loadContents str = 
  let ls = lines str
      parsedLines = mapMaybe parse ls in
  foldr (uncurry Map.insert) Map.empty parsedLines

  where
      parse :: String -> Maybe (String, String)
      parse l = undefined
        -- let ss = words l
        -- -- warning: unsafe head
        --     n = (readMaybe $ head ss) :: Maybe Int
        --     in n >>= \n' ->
        --       let inputs = take n' (drop 1 ss)
        --           outputs = drop (n' + 1) ss
        --           in (unwords inputs, unwords outputs)