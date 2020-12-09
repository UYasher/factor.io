module Main where

import Blueprint (blankBlueprint)
import Brick (customMain)
import Control.Monad (void)
import Graphics.Vty
import ResourceUpdate
import UI (UIState (UIState), app, boardHeight, boardWidth)

main :: IO ()
main = do
  let buildVty = do
        v <- mkVty =<< standardIOConfig
        setMode (outputIface v) Mouse True
        return v

  initialVty <- buildVty
  void $ customMain initialVty buildVty Nothing app b
  where
    b = UIState (blankBlueprint boardHeight boardWidth) Nothing emptyResources "Empty"
