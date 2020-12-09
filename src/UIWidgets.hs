module UIWidgets where

import Ascii
import Blueprint
import Brick.AttrMap
import Brick.Types hiding (Horizontal, Vertical)
import Brick.Widgets.Border as B
import Brick.Widgets.Border.Style as BS (unicodeBold)
import Brick.Widgets.Center as C
import Brick.Widgets.Core as WC
import Control.Applicative ((<|>))
import Geometry
import Machine
import Operator
import ResourceUpdate
import State (evalState)
import UIAttributes
import UITypes
import Wire

renderUI :: UIState -> [Widget Name]
renderUI uis =
  [ C.center $
      renderLeftBoard uis
        <+> renderFactory uis
        <+> padLeft (Pad 1) (renderSideBoard uis)
  ]

-- Render Info boxes on the Left
renderLeftBoard :: UIState -> Widget Name
renderLeftBoard UIState {ss = s, cl = l} =
  padRight (Pad 1) $
    clickable Run (renderSimpleBox "run")
      <=> clickable Random (renderSimpleBox "Random\npuzzle")
      <=> renderDebug l (renderSimpleBox s)
  where
    renderSimpleBox s =
      addBoldBorder "" . vLimit 3 . hLimit 12 . C.center $ str s

-- Render Selection boxes on the right
renderSideBoard :: UIState -> Widget Name
renderSideBoard UIState {cl = l} =
  opSelectBox <+> renderDebug l (padLeft (Pad 1) sinkSelectBox)
  where
    opSelectBox = addBoldBorder "" . machineList $ opMachines ++ wireMachines
    sinkSelectBox = addBoldBorder "Debug" $ machineList goalMachines -- Debug!
    machineList l = vBox [selectable m | m <- l]
    selectable m = clickable (Select m) $ renderMachineSelector m

renderMachineSelector :: Machine -> Widget n
renderMachineSelector m =
  machineAttr m (str $ drawMachine m) <+> showNumMachines Nothing

showNumMachines :: Maybe Int -> Widget n
showNumMachines Nothing =
  padLeft (Pad 1) (vLimit 3 $ C.vCenter $ str "-- inf")
showNumMachines (Just i) =
  padLeft (Pad 1) (vLimit 3 $ C.vCenter $ str $ "--   " ++ show i)

-- Render the center game board

renderFactory :: UIState -> Widget Name
renderFactory uis@UIState {bp = b} =
  addBoldBorder "Factory" . clickable Board $ vBox rows
  where
    rows = [hBox $ cellsInRow r | r <- [height b - 1, height b - 2 .. 0]]
    cellsInRow y = [renderCoord $ Point x y | x <- [0 .. width b - 1]]
    renderCoord p = renderCell p uis

renderCell :: Point -> UIState -> Widget n
renderCell p uis@UIState {bp = b, cr = r} =
  case cellTypeAt b p of
    Blueprint.Fixed -> str filled
    Empty -> str empty
    Machine m@(Sink _) -> sinkAttr m uis . str $ drawMachine m
    Machine m -> renderWithOverlay i m $ drawMachine m |*| i
  where
    h = evalState (getHoriz p) r
    v = evalState (getVert p) r
    i = h <|> v

renderWithOverlay :: Maybe Int -> Machine -> String -> Widget n
renderWithOverlay Nothing m s = machineAttr m $ str s
renderWithOverlay (Just i) m s =
  machineAttr m (str top)
    <=> ( machineAttr m (str midLeft)
            <+> withAttr flow (str num)
            <+> machineAttr m (str midRight)
        )
    <=> machineAttr m (str bot)
  where
    rows = lines s
    top = head rows
    mid = rows !! 1
    bot = rows !! 2
    midLeft = take 2 mid
    midRight = if i < 10 then drop 3 mid else drop 4 mid
    num = if i < 10 then take 1 $ drop 2 mid else take 2 $ drop 2 mid

-- General helper functions

addBoldBorder :: String -> Widget n -> Widget n
addBoldBorder s = withBorderStyle BS.unicodeBold . B.borderWithLabel (str s)

renderDebug :: Layer -> Widget n -> Widget n
renderDebug Debug w = w
renderDebug _ w = emptyWidget

-- | Lists of machines
opMachines :: [Machine]
opMachines = Op <$> [Add, Subtract, Multiply, Divide, Modulo, Factor, Duplicate]

wireMachines :: [Machine]
wireMachines = Wire <$> [Vertical, Horizontal, NW, NE, Overlap, SW, SE]

-- | Sinks and Sources. For Debugging purposes.
goalMachines :: [Machine]
goalMachines = [Sink 5, Source 10, Source 2]
