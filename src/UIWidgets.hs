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
renderUI uis@UIState {cl = l} =
  case l of
    Debug ->
      [ C.center $
          renderLeftBoard uis
            <+> renderFactory uis
            <+> padLeft (Pad 1) (renderSideBoard uis)
      ]
    Menu -> [menu]

-- Render Menu
menu :: Widget Name
menu =
  C.center (C.hCenter (str title) <=> C.hCenter (padTop (Pad 3) (clickable (Move Debug) (button "Sandbox Mode"))))

button :: String -> Widget Name
button s = addBoldBorder "" . vLimit 3 . hLimit 50 . C.center $ str s

-- Render Info boxes on the Left
renderLeftBoard :: UIState -> Widget Name
renderLeftBoard UIState {ss = s, cl = l} =
  padRight (Pad 1) $
    clickable Run (renderSimpleBox "" "run")
      <=> clickable Random (renderSimpleBox "" "Random puzzle")
      <=> renderDebug l (renderSimpleBox "Debug" s)
      <=> renderSimpleBox
        "Info"
        "m           -- menu \n\
        \space       -- clear \n\
        \right click -- erase \n\
        \q           -- quit  "
  where
    renderSimpleBox t s =
      addBoldBorder t . vLimit 3 . hLimit 23 . C.center $ str s

-- Render Selection boxes on the right
renderSideBoard :: UIState -> Widget Name
renderSideBoard UIState {cl = l} =
  opSelectBox <=> renderDebug l sinkSelectBox
  where
    opSelectBox = addBoldBorder "" (machineList $ opMachines ++ wireMachines)
    sinkSelectBox = addBoldBorder "Debug" $ machineList goalMachines -- Debug!
    machineList l = vBox [selectable m | m <- l]
    selectable m = clickable (Select m) $ renderMachineSelector m

renderMachineSelector :: Machine -> Widget n
renderMachineSelector m =
  withAttr (machineAttr m) (str $ drawMachine m) <+> showNumMachines Nothing

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
    Blueprint.Fixed -> withAttr highlight $ str filled
    Empty -> withAttr highlight $ str empty
    Machine m@(Sink _) ->
      withAttr (highlight <> sinkAttr m uis) . str $ drawMachine m
    Machine m ->
      withAttr (highlight <> machineAttr m)
        . renderWithOverlay i
        $ drawMachine m |*| i
  where
    highlight = renderPreWire p uis
    h = evalState (getHoriz p) r
    v = evalState (getVert p) r
    i = h <|> v

renderPreWire :: Point -> UIState -> AttrName
renderPreWire p UIState {hl = preWires} =
  if p `elem` preWires then preWire else mempty

renderWithOverlay :: Maybe Int -> String -> Widget n
renderWithOverlay Nothing s = str s
renderWithOverlay (Just i) s =
  str top
    <=> ( str midLeft
            <+> withAttr flow (str num)
            <+> str midRight
        )
    <=> str bot
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
opMachines = Op <$> (enumFrom minBound :: [Operator])

wireMachines :: [Machine]
wireMachines = [Wire Horizontal]

-- | Sinks and Sources. For Debugging purposes.
goalMachines :: [Machine]
goalMachines = [Sink 5, Source 10, Source 2]
