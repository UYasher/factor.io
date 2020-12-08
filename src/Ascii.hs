module Ascii where

import Brick
import Brick.Widgets.Center as C
import Machine
import Operator
import Wire

data Name = Board | Select {name :: Machine} | Run
  deriving (Eq, Ord)

drawMachinesLeft :: Int -> Widget n
drawMachinesLeft (-1) = padLeft (Pad 1) (vLimit 3 $ C.vCenter $ str "-- inf")
drawMachinesLeft i = padLeft (Pad 1) (vLimit 3 $ C.vCenter $ str $ "--   " ++ show i)

drawMachineSelector :: Machine -> Widget Name
drawMachineSelector m = clickable (Select m) $ drawMachine m <+> drawMachinesLeft (-1)

drawMachine :: Machine -> Widget n
drawMachine (Op op) = str $ drawOp (opToChar op)
drawMachine (Wire dir) = str $ drawWire dir
drawMachine Occupied = str drawOccupied
drawMachine (Source n) = drawSource n
drawMachine (Sink n) = drawSink n

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
  str " ┌ɅɅ┐ "
    <=> (str "<" <+> hLimit 4 (C.hCenter $ str $ show n) <+> str ">")
    <=> str " └VV┘ "

drawSink :: Int -> Widget n
drawSink n =
  str " ┌VV┐ "
    <=> (str ">" <+> hLimit 4 (C.hCenter $ str $ show n) <+> str "<")
    <=> str " └ɅɅ┘ "

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
