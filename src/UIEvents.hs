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
handleEvent uis@(UIState _ _ _ "Running") (AppEvent Tick) = continue $ stepUIState uis
handleEvent uis _ = continue uis

keyEvent :: UIState -> BrickEvent Name Tick -> EventM Name (Next UIState)
keyEvent uis (VtyEvent (EvKey (KChar 'q') [])) = halt uis
keyEvent _ (VtyEvent (EvKey (KChar 'r') [])) = liftIO initUIState >>= continue

mouseEvent :: UIState -> BrickEvent Name Tick -> EventM Name (Next UIState)
mouseEvent uis (MouseUp (Select m) _ _) = continue $ uis {selectedMachine = Just m}
mouseEvent (UIState b (Just m) _ _) (MouseUp Board (Just BLeft) l) = continue $ addToBoard l m b
mouseEvent uis (MouseUp Board (Just BRight) l) = continue $ rmFromBoard l uis (blueprint uis)
mouseEvent _ (MouseUp Random (Just BLeft) _) = liftIO (generate fakeRandomUIState) >>= continue
mouseEvent (UIState b p _ _) (MouseUp Run (Just BLeft) _) = continue $ UIState b p emptyResources "Running"

stepUIState :: UIState -> UIState
stepUIState (UIState b p r s) = UIState b p r' s'
  where
    r' = maybe r (`step` r) (makeFactory b)
    s' = maybe (status b) (\f -> if isStabilized f r' then status b else s) (makeFactory b)

addToBoard :: Location -> Machine -> Blueprint -> UIState
addToBoard l m b =
  case m of
    Sink _ -> UIState sb' (Just m) emptyResources (status sb')
    _ -> UIState b' (Just m) emptyResources (status b')
  where
    b' = placeMachineAt (tf l) m b
    sb' = addSink b'

rmFromBoard :: Location -> UIState -> Blueprint -> UIState
rmFromBoard l uis b =
  case getMachineAt (tf l) b of
    Nothing -> uis
    Just (Sink _) -> UIState sb' (selectedMachine uis) emptyResources (status sb')
    Just _ -> UIState b' (selectedMachine uis) emptyResources (status b')
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
tf (Location (x, y)) = Point (x `div` cellWidth) (boardHeight - 1 - (y `div` cellHeight))
  where
    cellWidth = 6
    cellHeight = 3

addSink :: Blueprint -> Blueprint
addSink b@Blueprint {minimumSinksToSatisfy = m} = b {minimumSinksToSatisfy = m + 1}

rmSink :: Blueprint -> Blueprint
rmSink b@Blueprint {minimumSinksToSatisfy = m} = b {minimumSinksToSatisfy = m - 1}

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
  return $ UIState b m r s
