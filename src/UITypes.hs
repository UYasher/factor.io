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
    hl :: [Point] -- Highlighter
  }

initUIState :: IO UIState
initUIState = do
  let b = blankBlueprint boardHeight boardWidth
  let m = Nothing
  let r = emptyResources
  let s = "Empty"
  let l = Menu
  let w = []
  return $ UIState b m r s l w

clearUIState :: UIState -> UIState
clearUIState uis = uis {bp = b, cr = r}
  where
    b = blankBlueprint boardHeight boardWidth
    r = emptyResources

data Tick = Tick

data Layer = Menu | Debug | Quit -- | Campaign | LevelSelect
  deriving (Eq, Ord)

data Name = Board | Run | Random | Select {name :: Machine} | Move {to :: Layer}
  deriving (Eq, Ord)

boardHeight, boardWidth :: Int
boardHeight = 15
boardWidth = 15
