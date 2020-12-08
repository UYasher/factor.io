module Ascii where

import Brick
import Brick.Widgets.Center as C
import Graphics.Vty as V
import Machine
import Operator
import Wire

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
