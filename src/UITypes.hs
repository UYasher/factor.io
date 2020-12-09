module UITypes where

import Ascii
import Blueprint
import Brick hiding (Horizontal, Vertical)
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
  { bp :: Blueprint, -- Blueprint
    sm :: Maybe Machine, -- Selected Machine
    cr :: Resources, -- Current Resource
    ss :: String, -- Status String
    cl :: Layer, -- Current Layer
    wd :: UIWireDirection -- User Wire Placement
  }

initUIState :: IO UIState
initUIState = do
  let b = blankBlueprint boardHeight boardWidth
  let m = Nothing
  let r = emptyResources
  let s = "Empty"
  let l = Debug
  let w = NS
  return $ UIState b m r s l w

data Tick = Tick

data Layer = Menu | Campaign | Debug

data UIWireDirection = NS | EW

data Name = Board | Select {name :: Machine} | Run | Random
  deriving (Eq, Ord)

boardHeight, boardWidth :: Int
boardHeight = 15
boardWidth = 15
