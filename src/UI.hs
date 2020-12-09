module UI where

import Ascii
import Blueprint
import Brick hiding (Horizontal, Vertical)
import Brick.Types as BT hiding (Horizontal, Vertical)
import Brick.Util (fg, on)
import Brick.Widgets.Border as B
import Brick.Widgets.Border.Style as BS
import Brick.Widgets.Center as C
import Brick.Widgets.Core as WC
import Data.Maybe (fromMaybe)
import Factory
import Geometry
import Graphics.Vty as V
import Machine
import Operator
import ResourceUpdate
import State (evalState)
import Wire

-- | We need a wrapper around the Blueprint because
-- | we want to keep track of which machines are currently
-- | selected by the player
data UIState = UIState
  { blueprint :: Blueprint,
    selectedMachine :: Maybe Machine,
    currResource :: Resources,
    statusString :: String
  }

data Tick = Tick

data Name = Board | Select {name :: Machine} | Run
  deriving (Eq, Ord)

app :: App UIState Tick Name
app =
  App
    { appDraw = renderUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return,
      appAttrMap = const theMap
    }

boardHeight, boardWidth :: Int
boardHeight = 15
boardWidth = 15

wire, operator, occupied, source, sink :: AttrName
wire = attrName "wire"
operator = attrName "operator"
source = attrName "source"
sink = attrName "sink"
occupied = attrName "occupied"

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (wire, fg red),
      (occupied, fg brightRed),
      (operator, fg brightBlue),
      (source, fg brightYellow),
      (sink, fg brightGreen)
    ]

renderUI :: UIState -> [Widget Name]
renderUI uis = [C.center $ renderLeftBoard uis <+> renderFactory uis <+> renderSideBoard uis]

-- For debugging and running
renderLeftBoard :: UIState -> Widget Name
renderLeftBoard UIState {statusString = s} =
  padRight (Pad 1) $
    ( withBorderStyle BS.unicodeBold
        . B.borderWithLabel (str "Debug")
        . vLimit 3
        . hLimit 7
        . C.center
        $ str s
    )
      <=> ( clickable Run
              . withBorderStyle BS.unicodeBold
              . B.border
              . vLimit 3
              . hLimit 7
              . C.center
              $ str "Run"
          )

-- For item selection
renderSideBoard :: UIState -> Widget Name
renderSideBoard _ =
  padLeft (Pad 2) $
    withBorderStyle
      BS.unicodeBold
      (B.border $ vBox [renderClickable m | m <- opMachines ++ wireMachines])
      <=> withBorderStyle
        BS.unicodeBold
        (B.borderWithLabel (str "debug") $ vBox [renderClickable m | m <- goalMachines])
  where
    renderClickable m = clickable (Select m) $ renderMachineSelector m

showNumMachines :: Int -> Widget n
showNumMachines (-1) = padLeft (Pad 1) (vLimit 3 $ C.vCenter $ str "-- inf")
showNumMachines i = padLeft (Pad 1) (vLimit 3 $ C.vCenter $ str $ "--   " ++ show i)

renderMachineSelector :: Machine -> Widget n
renderMachineSelector m = machineAttr m (str $ drawMachine m) <+> showNumMachines (-1)

renderFactory :: UIState -> Widget Name
renderFactory uis@UIState {blueprint = b} =
  withAttr (attrName "colorful")
    . withBorderStyle BS.unicodeBold
    . B.borderWithLabel (str "Factory")
    . clickable Board
    $ vBox rows
  where
    rows = [hBox $ cellsInRow r | r <- [height b - 1, height b - 2 .. 0]]
    cellsInRow y = [renderCoord $ Point x y | x <- [0 .. width b - 1]]
    renderCoord p = renderCell p uis

renderCell :: Point -> UIState -> Widget n
renderCell p UIState {blueprint = b, currResource = r} =
  case cellTypeAt b p of
    Blueprint.Fixed -> str filled
    Empty -> str empty
    Machine m -> machineAttr m . str $ drawMachine m |*| h |*| v
  where
    h = fromMaybe (-1) $ getHorzIntAt p r
    v = fromMaybe (-1) $ getVertIntAt p r

machineAttr :: Machine -> Widget n -> Widget n
machineAttr m =
  case m of
    Op _ -> withAttr operator
    Wire _ -> withAttr wire
    Occupied -> withAttr occupied
    Source _ -> withAttr source
    Sink _ -> withAttr sink

getHorzIntAt :: Point -> Resources -> Maybe Int
getHorzIntAt p = evalState (getHoriz p)

getVertIntAt :: Point -> Resources -> Maybe Int
getVertIntAt p = evalState (getVert p)

handleEvent :: UIState -> BrickEvent Name Tick -> EventM Name (Next UIState)
handleEvent uis (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt uis
handleEvent uis (MouseDown (Select m) _ _ _) = continue $ uis {selectedMachine = Just m}
handleEvent (UIState b p@(Just m) _ _) (MouseUp Board (Just BLeft) l) = continue $ addToBoard l m b
handleEvent uis (MouseUp Board (Just BRight) l) = continue $ rmFromBoard l uis (blueprint uis)
handleEvent (UIState b p _ _) (MouseUp Run (Just BLeft) _) = continue $ UIState b p emptyResources (status b)
handleEvent uis _ = continue uis

addToBoard :: Location -> Machine -> Blueprint -> UIState
addToBoard l m b =
  case m of
    Sink _ -> UIState sb' (Just m) emptyResources (status sb')
    _ -> UIState b' (Just m) emptyResources (status b')
  where
    b' = placeMachineAt (tf l) m b
    sb' = addSink b'

rmFromBoard :: Location -> UIState -> Blueprint -> UIState
rmFromBoard l uis b =
  case getMachineAt (tf l) b of
    Nothing -> uis
    Just (Sink _) -> UIState sb' (selectedMachine uis) emptyResources (status sb')
    Just _ -> UIState b' (selectedMachine uis) emptyResources (status b')
  where
    b' = removeMachineAt (tf l) b
    sb' = rmSink b'

status :: Blueprint -> String
status b =
  if minimumSinksToSatisfy b == 0
    then "Empty"
    else case makeFactory b of
      Just f -> show $ isSatisfied b $ stepUntilStableOrN 50 f emptyResources
      Nothing -> "Illegal arrangement"

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

-- | Lists of machines
opMachines :: [Machine]
opMachines = Op <$> [Add, Subtract, Multiply, Divide, Modulo, Factor, Duplicate]

wireMachines :: [Machine]
wireMachines = Wire <$> [Vertical, Horizontal, NE, SE, NW, SW, Overlap]

-- | Sinks and Sources. For Debugging purposes.
goalMachines :: [Machine]
goalMachines = [Sink 5, Source 10, Source 2]
