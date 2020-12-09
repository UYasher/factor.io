module UITypes where

import Ascii
import Blueprint
import Brick hiding (Horizontal, Vertical)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Factory
import Geometry
import Graphics.Vty as V
import Machine
import Operator
import ResourceUpdate
import Test.QuickCheck.Gen
import Wire

data UIState = UIState
  { blueprint :: Blueprint,
    selectedMachine :: Maybe Machine,
    currResource :: Resources,
    statusString :: String
  }

initUIState :: IO UIState
initUIState = do
  let b = blankBlueprint boardHeight boardWidth
  let m = Nothing
  let r = emptyResources
  let s = "Empty"
  return $ UIState b m r s

data Tick = Tick

data Name = Board | Select {name :: Machine} | Run | Random
  deriving (Eq, Ord)

boardHeight, boardWidth :: Int
boardHeight = 15
boardWidth = 15
