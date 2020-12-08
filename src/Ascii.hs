module Ascii where

import Brick
import Brick.Widgets.Center as C
import Graphics.Vty as V
import Machine
import Operator
import Wire

wire, operator, source, sink :: AttrName
wire = attrName "wire"
operator = attrName "operator"
source = attrName "source"
sink = attrName "sink"

occupied = attrName "occupied"

aMap :: AttrMap
aMap =
  attrMap
    V.defAttr
    [ (wire, fg red),
      (occupied, fg brightRed),
      (operator, fg brightBlue),
      (source, fg brightYellow),
      (sink, fg brightGreen)
    ]

drawMachinesLeft :: Int -> Widget n
drawMachinesLeft (-1) = padLeft (Pad 1) (vLimit 3 $ C.vCenter $ str "-- inf")
drawMachinesLeft i = padLeft (Pad 1) (vLimit 3 $ C.vCenter $ str $ "--   " ++ show i)

drawMachineSelector :: Machine -> Widget n
drawMachineSelector m = drawMachine m <+> drawMachinesLeft (-1)

drawMachine :: Machine -> Widget n
drawMachine (Op op) = withAttr operator . str $ drawOp (opToChar op)
drawMachine (Wire dir) = withAttr wire . str $ drawWire dir
drawMachine Occupied = str drawOccupied
drawMachine (Source n) = withAttr source $ drawSource n
drawMachine (Sink n) = withAttr sink $ drawSink n

-- For future use
-- drawMachine _ = drawGeneric

drawOp :: Char -> String
drawOp c =
  " ---- \n\
  \| "
    ++ [c, c]
    ++ " |\n\
       \ ----"

testMachine :: String
testMachine =
  " ---- \n\
  \| {} |\n\
  \ ----"

drawWire :: WireType -> String
drawWire Wire.Vertical =
  "  ││  \n\
  \  ││  \n\
  \  ││  "
drawWire Wire.Horizontal =
  "      \n\
  \══════\n\
  \      "
drawWire NE =
  "  ││  \n\
  \  ╘╧══\n\
  \      "
drawWire SE =
  "      \n\
  \  ════\n\
  \  ││  "
drawWire SW =
  "      \n\
  \════  \n\
  \  ││  "
drawWire NW =
  "  ││  \n\
  \════  \n\
  \      "
drawWire Overlap =
  "  ││  \n\
  \══════\n\
  \  ││  "

drawOccupied :: String
drawOccupied =
  "\\\\\\\\\\\\\n\
  \//////\n\
  \\\\\\\\\\\\\"

drawSource :: Int -> Widget n
drawSource n =
  str "┌────┐"
    <=> (str "│" <+> hLimit 4 (C.hCenter $ str $ show n) <+> str "│")
    <=> str "└\\VV/┘"

drawSink :: Int -> Widget n
drawSink n =
  str "┌\\VV/┐"
    <=> (str "│" <+> hLimit 4 (C.hCenter $ str $ show n) <+> str "│")
    <=> str "└────┘"

drawGeneric :: String
drawGeneric =
  " PLAC \n\
  \EHOLDE\n\
  \ R--- "

filled :: String
filled =
  "XXXXXX\n\
  \XXXXXX\n\
  \XXXXXX"

empty :: String
empty =
  "      \n\
  \      \n\
  \      "
