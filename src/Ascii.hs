module Ascii where

import Brick
import Machine
import Operator
import Wire

-- For future use
-- drawMachine _ = drawGeneric

-- DIFFERENCE BETWEEN "DRAW" AND "RENDER":
-- DRAW :: String
-- RENDER :: Widget n

-- | Add a number to the center of a sprite
-- | Note that 0 <= i <= 99, and len(str) == 21
(|*|) :: String -> Int -> String
s |*| i | i < 0 = s
s |*| i | i < 10 = take 9 s ++ show i ++ drop 10 s
s |*| i = take 9 s ++ show i ++ drop 11 s

drawMachine :: Machine -> String
drawMachine m =
  case m of
    Op op -> drawOp (opToChar op)
    Wire dir -> drawWire dir
    Occupied -> drawOccupied
    Source n -> drawSource n
    Sink n -> drawSink n

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

drawSource :: Int -> String
drawSource n
  | n < 10 =
    unlines
      [ "┌────┐",
        "│ " ++ show n ++ "  │",
        "└\\VV/┘"
      ]
  | otherwise =
    unlines
      [ "┌────┐",
        "│ " ++ show n ++ " │",
        "└\\VV/┘"
      ]

drawSink :: Int -> String
drawSink n
  | n < 10 =
    unlines
      [ "┌\\VV/┐",
        "│ " ++ show n ++ "  │",
        "└────┘"
      ]
  | otherwise =
    unlines
      [ "┌\\VV/┐",
        "│ " ++ show n ++ " │",
        "└────┘"
      ]

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
