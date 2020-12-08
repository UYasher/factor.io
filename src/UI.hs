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
import Factory
import Geometry
import Graphics.Vty as V
import Machine
import Operator
import ResourceUpdate (emptyResources)

-- | We need a wrapper around the Blueprint because
-- | we want to keep track of which machines are currently
-- | selected by the player
data UIState = UIState
  { blueprint :: Blueprint,
    placeMachine :: Maybe Machine,
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

main :: IO ()
main = do
  let buildVty = do
        v <- V.mkVty =<< V.standardIOConfig
        V.setMode (V.outputIface v) V.Mouse True
        return v

  initialVty <- buildVty
  void $ customMain initialVty buildVty Nothing app b
  where
    b = UIState (blankBlueprint 12 12) Nothing "Hm.."

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
drawCell Blueprint.Fixed = filled
drawCell Empty = empty
drawCell (Machine m) = drawMachine m

handleEvent :: UIState -> BrickEvent Name () -> EventM Name (Next UIState)
handleEvent uis (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt uis
handleEvent uis (MouseDown (Select m) _ _ _) = continue $ uis {placeMachine = Just m}
handleEvent uis@UIState {blueprint = b, placeMachine = Just m} (MouseUp Board (Just BLeft) l) = continue $ uis {blueprint = placeMachineAt (tf l b) m b}
handleEvent uis@UIState {blueprint = b} (MouseUp Board (Just BRight) l) = continue $ uis {blueprint = removeMachineAt (tf l b) b}
handleEvent uis@UIState {blueprint = b, statusString = s} (MouseUp Run (Just BLeft) _) =
  case makeFactory b of
    Just f -> if isSatisfied b (stepUntilStableOrN 0 f emptyResources) then continue $ uis {statusString = "Solved!"} else continue uis
    Nothing -> continue uis
handleEvent uis _ = continue uis

aMap :: AttrMap
aMap = attrMap V.defAttr []
