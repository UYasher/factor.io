module UIAttributes where

import Blueprint
import Brick
import Brick.AttrMap
import Graphics.Vty as V
import Machine
import UITypes

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (wire, fg brightYellow),
      (occupied, fg brightRed),
      (operator, fg brightBlue),
      (source, fg brightYellow),
      (sink, fg red),
      (solvedSink, fg brightGreen),
      (flow, fg brightWhite)
    ]

wire, operator, occupied, source, sink, solvedSink, flow :: AttrName
wire = attrName "wire"
operator = attrName "operator"
source = attrName "source"
sink = attrName "sink"
solvedSink = attrName "solvedSink"
occupied = attrName "occupied"
flow = attrName "flow"

sinkAttr :: Machine -> UIState -> Widget n -> Widget n
sinkAttr (Sink _) UIState {blueprint = b, currResource = r} =
  if isSatisfied b r then withAttr solvedSink else withAttr sink

machineAttr :: Machine -> Widget n -> Widget n
machineAttr m =
  case m of
    Op _ -> withAttr operator
    Wire _ -> withAttr wire
    Occupied -> withAttr occupied
    Source _ -> withAttr source
    Sink _ -> withAttr sink
