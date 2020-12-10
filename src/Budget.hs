module Budget where

import qualified Data.Map as Map
import Machine
import Operator

-- | Represents restrictions on the number of different types of items used to
-- solve a puzzle, as a way of making puzzles more or less challenging
type Budget = Map.Map String Int

get :: String -> Budget -> Int
get s = (`orElse` 0) . Map.lookup s
  where
    Just x `orElse` _ = x
    Nothing `orElse` y = y

set :: String -> Int -> Budget -> Budget
set = Map.insert

-- | The budget with all items at 0
zeroBudget :: Budget
zeroBudget = Map.empty

-- | Returns `True` iff every element of the first argument is pairwise less than
-- or equal to the element from the second argument
isWithin :: Budget -> Budget -> Bool
a `isWithin` b = all (\f -> get f a <= get f b) $ Map.keys a
