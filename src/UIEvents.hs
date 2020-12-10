module UIEvents where

import Blueprint
import Brick.Main
import Brick.Types
import Control.Monad.IO.Class (liftIO)
import Factory
import Geometry
import Graphics.Vty
import Machine
import ResourceUpdate
import Test.QuickCheck.Gen
import UITypes

handleEvent :: UIState -> BrickEvent Name Tick -> EventM Name (Next UIState)
handleEvent uis e@(VtyEvent (EvKey _ _)) = keyEvent uis e
handleEvent uis e@MouseUp {} = mouseEvent uis e
handleEvent uis@UIState {ss = "Running"} (AppEvent Tick) =
  continue $ stepUIState uis
handleEvent uis _ = continue uis

keyEvent :: UIState -> BrickEvent Name Tick -> EventM Name (Next UIState)
keyEvent uis (VtyEvent (EvKey (KChar 'q') [])) = halt uis
<<<<<<< HEAD
=======
keyEvent uis@UIState {wd = NS} (VtyEvent (EvKey (KChar 'f') [])) = continue $ uis {wd = EW}
keyEvent uis@UIState {wd = EW} (VtyEvent (EvKey (KChar 'f') [])) = continue $ uis {wd = NS}
>>>>>>> f246e7fd343e0e6b2a2e2969f56470724fbf139c
keyEvent _ (VtyEvent (EvKey (KChar 'r') [])) = liftIO initUIState >>= continue
keyEvent uis _ = continue uis

mouseEvent :: UIState -> BrickEvent Name Tick -> EventM Name (Next UIState)
mouseEvent uis (MouseUp (Select m) _ _) = continue $ uis {sm = Just m}
mouseEvent uis@UIState {bp = b, sm = (Just m)} (MouseUp Board (Just BLeft) l) =
  continue $ addToBoard l uis m b
mouseEvent uis (MouseUp Board (Just BRight) l) =
  continue $ rmFromBoard l uis (bp uis)
mouseEvent _ (MouseUp Random (Just BLeft) _) =
  liftIO (generate fakeRandomUIState) >>= continue
mouseEvent uis@UIState {bp = b, sm = p, cl = l} (MouseUp Run (Just BLeft) _) =
  continue $ uis {cr = emptyResources, ss = "Running"}

stepUIState :: UIState -> UIState
stepUIState uis@UIState {bp = b, cr = r, ss = s} = uis {cr = r', ss = s'}
  where
    r' = maybe r (`step` r) (makeFactory b)
    s' = maybe (status b) checkIfRunning (makeFactory b)
    checkIfRunning = \f -> if isStabilized f r' then status b else s

addToBoard :: Location -> UIState -> Machine -> Blueprint -> UIState
addToBoard l uis m b =
  case m of
    Sink _ ->
      uis
        { bp = sb',
          sm = Just m,
          cr = emptyResources,
          ss = status sb'
        }
    _ ->
      uis
        { bp = b',
          sm = Just m,
          cr = emptyResources,
          ss = status b'
        }
  where
    b' = placeMachineAt (tf l) m b
    sb' = addSink b'

rmFromBoard :: Location -> UIState -> Blueprint -> UIState
rmFromBoard l uis b =
  case getMachineAt (tf l) b of
    Nothing -> uis
    Just (Sink _) -> uis {bp = sb', cr = emptyResources, ss = status sb'}
    Just _ -> uis {bp = b', cr = emptyResources, ss = status sb'}
  where
    b' = removeMachineAt (tf l) b
    sb' = rmSink b'

status :: Blueprint -> String
status b =
  if minimumSinksToSatisfy b == 0
    then "Empty"
    else case makeFactory b of
      Just f -> show $ isSatisfied b $ stepUntilStableOrN 50 f emptyResources
      Nothing -> "Illegal arrangement"

-- | Transforms Brick Widget Locations (on a square grid) into blueprint points
tf :: Location -> Point
tf (Location (x, y)) =
  Point (x `div` cellWidth) (boardHeight - 1 - (y `div` cellHeight))
  where
    cellWidth = 6
    cellHeight = 3

-- | Add a sink, for sandbox mode/debugging
addSink :: Blueprint -> Blueprint
addSink b@Blueprint {minimumSinksToSatisfy = m} =
  b {minimumSinksToSatisfy = m + 1}

-- | Remove a sink, also for sandbox mode/debugging
rmSink :: Blueprint -> Blueprint
rmSink b@Blueprint {minimumSinksToSatisfy = m} =
  b {minimumSinksToSatisfy = m - 1}

-- | for demo purposes
fakeRandomPuzzle :: Gen Blueprint
fakeRandomPuzzle = do
  numIns <- elements [1, 2]
  numOuts <- elements [1, 2]
  let sources = mapM (\_ -> Source <$> choose (0, 63)) [0 .. numIns]
  let sinks = mapM (\_ -> Sink <$> choose (0, 63)) [0 .. numOuts]
  let sourceLocations = [Point 2 14, Point 12 14]
  let sinkLocations = [Point 5 0, Point 10 0]
  let sourceFs = zipWith placeMachineAt sourceLocations <$> sources
  let sinkFs = zipWith placeMachineAt sinkLocations <$> sinks
  let allModifications = foldr (.) <$> (foldr (.) id <$> sourceFs) <*> sinkFs
  allModifications <*> return (addSink $ blankBlueprint 15 15)

-- | for demo purposes
fakeRandomUIState :: Gen UIState
fakeRandomUIState = do
  b <- fakeRandomPuzzle
  let m = Nothing
  let r = emptyResources
  let s = "Hello!"
  let l = Debug
  let w = []
  return $ UIState b m r s l w
