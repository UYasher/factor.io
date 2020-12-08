module UI where

import Ascii
import Blueprint
import Brick
import Brick.Types as BT
import Brick.Widgets.Border as B
import Brick.Widgets.Border.Style as BS
import Brick.Widgets.Center as C
import Brick.Widgets.Core as WC
import Control.Monad (void)
import Geometry
import Graphics.Vty as V
import Machine
import Operator

data Tick = Tick

-- | We need a wrapper around the Blueprint because
-- | we want to keep track of which machines are currently
-- | selected by the player
data UIState = UIState
  { blueprint :: Blueprint,
    placeMachine :: Maybe Machine
  }

app :: App UIState Tick Name
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return,
      appAttrMap = const aMap
    }

main :: IO ()
main = do
  let buildVty = do
        v <- V.mkVty =<< V.standardIOConfig
        V.setMode (V.outputIface v) V.Mouse True
        return v

  initialVty <- buildVty
  void $ customMain initialVty buildVty Nothing app b
  where
    b = UIState (blankBlueprint 12 12) Nothing

blueprint1 :: Blueprint
blueprint1 = addFixedPoint (Point 2 2) $ blankBlueprint 11 11

drawUI :: UIState -> [Widget Name]
drawUI uis = [C.center $ drawFactory uis <+> padLeft (Pad 2) (drawSideBoard uis)]

drawSideBoard :: UIState -> Widget Name
drawSideBoard UIState {blueprint = b} =
  withBorderStyle BS.unicodeBold $ B.border $ vBox [drawMachineSelector m | m <- opMachines]

machineCount :: Widget Name
machineCount = padLeftRight 1 $ drawOp '+' <+> padLeft (Pad 1) (vLimit 3 $ C.vCenter $ str "-- inf")

drawFactory :: UIState -> Widget Name
drawFactory uis =
  withBorderStyle BS.unicodeBold $
    B.borderWithLabel (str "Factory") $
      clickable Board $ vBox rows
  where
    rows = [hBox $ cellsInRow r | r <- [height (blueprint uis) - 1, height (blueprint uis) - 2 .. 0]]
    cellsInRow y = [drawCoord $ Point x y | x <- [0 .. width (blueprint uis) - 1]]
    drawCoord = drawCell . cellTypeAt (blueprint uis)

drawCell :: CellType -> Widget Name
drawCell Blueprint.Fixed = filled
drawCell Empty = empty
drawCell (Machine m) = drawMachine m

handleEvent :: UIState -> BrickEvent Name Tick -> EventM Name (Next UIState)
handleEvent uis (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt uis
handleEvent uis (MouseDown (Select m) _ _ _) = continue $ uis {placeMachine = Just m}
handleEvent uis@UIState {blueprint = b, placeMachine = Just m} (MouseUp Board (Just BLeft) l) = continue $ uis {blueprint = placeMachineAt (tf l b) m b}
handleEvent uis _ = continue uis

aMap :: AttrMap
aMap = attrMap V.defAttr []
