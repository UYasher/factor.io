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
      (flow, fg brightWhite),
      (preWire, bg yellow)
    ]

wire, operator, occupied, source, sink, solvedSink, flow, preWire :: AttrName
wire = attrName "wire"
operator = attrName "operator"
source = attrName "source"
sink = attrName "sink"
solvedSink = attrName "solvedSink"
occupied = attrName "occupied"
flow = attrName "flow"
preWire = attrName "preWire"

sinkAttr :: Machine -> UIState -> AttrName
sinkAttr (Sink _) UIState {bp = b, cr = r} =
  if isSatisfied b r then solvedSink else sink

machineAttr :: Machine -> AttrName
machineAttr m =
  case m of
    Op _ -> operator
    Wire _ -> wire
    Occupied -> occupied
    Source _ -> source
    Sink _ -> sink
