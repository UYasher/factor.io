module Main where

import Blueprint (blankBlueprint)
import Brick (customMain)
import Brick.BChan (newBChan, writeBChan)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Graphics.Vty
import ResourceUpdate
import UI (Tick (Tick), UIState (UIState), app, boardHeight, boardWidth)

main :: IO ()
main = do
  let buildVty = do
        v <- mkVty =<< standardIOConfig
        setMode (outputIface v) Mouse True
        return v

  chan <- newBChan 10
  forkIO . forever $ do
    writeBChan chan Tick
    threadDelay 300000

  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just chan) app b
  where
    b = UIState (blankBlueprint boardHeight boardWidth) Nothing emptyResources "Empty"
