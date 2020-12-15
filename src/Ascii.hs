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
(|*|) :: String -> Maybe Int -> String
s |*| Nothing = s
s |*| (Just i)
  | i < 10 = take 9 s ++ show i ++ drop 10 s
  | otherwise = take 9 s ++ show i ++ drop 11 s

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
  \  ╒╤══\n\
  \  ││  "
drawWire SW =
  "      \n\
  \══╤╕  \n\
  \  ││  "
drawWire NW =
  "  ││  \n\
  \══╧╛  \n\
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

eraser :: String
eraser =
  "      \n\
  \ERASER\n\
  \      "

title :: String
title =
  "    ______________    ___________________  ______         ________ _____    \n\
  \   /   ____/  _   |  /  ___/__   __/ __  \\/  __  \\       /__   __ / __  \\   \n\
  \  /   ____/  /_|  | /  /     /  / / /  / /  /_/  /         /  /  / /  / /   \n\
  \ /   /   /  ___   |/  /___  /  / / /__/ /  / /  /   __  __/  /__/ /__/ /    \n\
  \/___/   /__/   |__/\\_____/ /__/  \\_____/__/  \\__\\  /_/ /_______/\\_____/    "
