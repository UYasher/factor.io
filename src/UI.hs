module UI where

import Ascii
import Blueprint
import Brick hiding (Horizontal, Vertical)
import Brick.Types as BT hiding (Horizontal, Vertical)
import Brick.Widgets.Border as B
import Brick.Widgets.Border.Style as BS
import Brick.Widgets.Center as C
import Brick.Widgets.Core as WC
import Control.Monad (void)
import Factory
import Geometry
import Graphics.Vty as V
import Machine
import Operator
import ResourceUpdate (emptyResources)
import Wire

-- | We need a wrapper around the Blueprint because
-- | we want to keep track of which machines are currently
-- | selected by the player
data UIState = UIState
  { blueprint :: Blueprint,
    selectedMachine :: Maybe Machine,
    statusString :: String
  }

app :: App UIState () Name
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return,
      appAttrMap = const aMap
    }

boardHeight, boardWidth :: Int
boardHeight = 15
boardWidth = 15

main :: IO ()
main = do
  let buildVty = do
        v <- V.mkVty =<< V.standardIOConfig
        V.setMode (V.outputIface v) V.Mouse True
        return v

  initialVty <- buildVty
  void $ customMain initialVty buildVty Nothing app b
  where
    b = UIState (blankBlueprint boardHeight boardWidth) Nothing "Hm.."

drawUI :: UIState -> [Widget Name]
drawUI uis = [C.center $ drawLeftBoard uis <+> drawFactory uis <+> drawSideBoard uis]

-- For debugging and running
drawLeftBoard :: UIState -> Widget Name
drawLeftBoard UIState {statusString = s} =
  padRight (Pad 1) $
    withBorderStyle
      BS.unicodeBold
      (B.border $ clickable Run $ vLimit 3 $ hLimit 7 $ C.center $ str "Run")
      <=> withBorderStyle BS.unicodeBold (B.borderWithLabel (str "Status") $ vLimit 3 $ hLimit 7 $ C.center $ str s)

-- For item selection
drawSideBoard :: UIState -> Widget Name
drawSideBoard _ =
  padLeft (Pad 2) $
    withBorderStyle
      BS.unicodeBold
      (B.border $ vBox [drawMachineSelector m | m <- opMachines ++ wireMachines])
      <=> withBorderStyle
        BS.unicodeBold
        (B.borderWithLabel (str "debug") $ vBox [drawMachineSelector m | m <- goalMachines])

drawFactory :: UIState -> Widget Name
drawFactory UIState {blueprint = b} =
  withBorderStyle BS.unicodeBold $
    B.borderWithLabel (str "Factory") $
      clickable Board $ vBox rows
  where
    rows = [hBox $ cellsInRow r | r <- [height b - 1, height b - 2 .. 0]]
    cellsInRow y = [drawCoord $ Point x y | x <- [0 .. width b - 1]]
    drawCoord = drawCell . cellTypeAt b

drawCell :: CellType -> Widget Name
drawCell Blueprint.Fixed = str filled
drawCell Empty = str empty
drawCell (Machine m) = drawMachine m

handleEvent :: UIState -> BrickEvent Name () -> EventM Name (Next UIState)
handleEvent uis (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt uis
handleEvent uis (MouseDown (Select m) _ _ _) = continue $ uis {selectedMachine = Just m}
handleEvent (UIState b p@(Just m) _) (MouseUp Board (Just BLeft) l) = continue $ addToBoard l m b
handleEvent uis (MouseUp Board (Just BRight) l) = continue $ rmFromBoard l uis (blueprint uis)
handleEvent (UIState b p _) (MouseUp Run (Just BLeft) _) = continue $ UIState b p (status b)
handleEvent uis _ = continue uis

addToBoard :: Location -> Machine -> Blueprint -> UIState
addToBoard l m b =
  case m of
    Sink _ -> UIState sb' (Just m) (status sb')
    _ -> UIState b' (Just m) (status b')
  where
    b' = placeMachineAt (tf l) m b
    sb' = addSink b'

rmFromBoard :: Location -> UIState -> Blueprint -> UIState
rmFromBoard l uis b =
  case getMachineAt (tf l) b of
    Nothing -> uis
    Just (Sink _) -> UIState sb' (selectedMachine uis) (status sb')
    Just _ -> UIState b' (selectedMachine uis) (status b')
  where
    b' = removeMachineAt (tf l) b
    sb' = rmSink b'

status :: Blueprint -> String
status b =
  case makeFactory b of
    Just f -> show $ isSatisfied b $ stepUntilStableOrN 50 f emptyResources
    Nothing -> "Illegal arrangement"

aMap :: AttrMap
aMap = attrMap V.defAttr []

-- | Transforms Brick Widget Locations (on a square grid) into brueprint points
tf :: Location -> Point
tf (Location (x, y)) = Point (x `div` cellWidth) (boardHeight - 1 - (y `div` cellHeight))
  where
    cellWidth = 6
    cellHeight = 3

addSink :: Blueprint -> Blueprint
addSink b@Blueprint {minimumSinksToSatisfy = m} = b {minimumSinksToSatisfy = m + 1}

rmSink :: Blueprint -> Blueprint
rmSink b@Blueprint {minimumSinksToSatisfy = m} = b {minimumSinksToSatisfy = m - 1}

-- | Returns a list of all operator machines
opMachines :: [Machine]
opMachines = Op <$> [Add, Subtract, Multiply, Divide, Modulo, Factor, Duplicate]

wireMachines :: [Machine]
wireMachines = Wire <$> [Vertical, Horizontal, NE, SE, SW, NW, Overlap]

-- | Sinks and Sources. For Debugging purposes.
goalMachines :: [Machine]
goalMachines = [Source 10, Source 2, Sink 5]
