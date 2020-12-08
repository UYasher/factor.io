module Ascii where

import Brick
import Brick.Widgets.Center as C
import Machine
import Operator
import Wire

data Name = Board | Select {name :: Machine}
  deriving (Eq, Ord)

drawMachinesLeft :: Int -> Widget n
drawMachinesLeft (-1) = padLeft (Pad 1) (vLimit 3 $ C.vCenter $ str "-- inf")
drawMachinesLeft i = padLeft (Pad 1) (vLimit 3 $ C.vCenter $ str $ "--   " ++ show i)

drawMachineSelector :: Machine -> Widget Name
drawMachineSelector m = clickable (Select m) $ drawMachine m <+> drawMachinesLeft (-1)

drawMachine :: Machine -> Widget n
drawMachine (Op op) = drawOp (opToChar op)
drawMachine (Wire dir) = drawWire dir
drawMachine Occupied = drawOccupied
drawMachine (Source n) = drawSource n
drawMachine _ = drawGeneric

drawOp :: Char -> Widget n
drawOp c =
  str $
    " ---- \n\
    \| "
      ++ [c, c]
      ++ " |\n\
         \ ----"

testMachine :: Widget n
testMachine =
  str
    " ---- \n\
    \| {} |\n\
    \ ----"

drawWire :: WireType -> Widget n
drawWire Wire.Vertical =
  str
    " -||- \n\
    \| || |\n\
    \ -||- "
drawWire Wire.Horizontal =
  str
    " ---- \n\
    \======\n\
    \ ---- "
drawWire NE =
  str
    " -||- \n\
    \| ====\n\
    \ ---- "
drawWire SE =
  str
    " ---- \n\
    \| ====\n\
    \ -||- "
drawWire SW =
  str
    " ---- \n\
    \==== |\n\
    \ -||- "
drawWire NW =
  str
    " -||- \n\
    \==== |\n\
    \ ---- "
drawWire Overlap =
  str
    " -||- \n\
    \======\n\
    \ -||- "

drawOccupied :: Widget n
drawOccupied =
  str
    "\\\\\\\\\\\\\n\
    \//////\n\
    \\\\\\\\\\\\\"

drawSource :: Int -> Widget n
drawSource n =
  str " -++- "
    <=> C.hCenter (str $ show n)
    <=> str " -++- "

drawSink :: Int -> Widget n
drawSink n =
  str
    " -00- \n\
    \000000\n\
    \ -00- "

drawGeneric :: Widget n
drawGeneric =
  str
    " PLAC \n\
    \EHOLDE\n\
    \ R--- "

filled :: Widget n
filled =
  str
    "XXXXXX\n\
    \XXXXXX\n\
    \XXXXXX"

empty :: Widget n
empty =
  str
    "┌────┐\n\
    \│    │\n\
    \└────┘"
