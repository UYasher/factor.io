module Main where

import Brick
import Brick.BChan (newBChan, writeBChan)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Graphics.Vty
import UIAttributes
import UIEvents
import UITypes
import UIWidgets

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
  b <- initUIState
  void $ customMain initialVty buildVty (Just chan) app b

app :: App UIState Tick Name
app =
  App
    { appDraw = renderUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return,
      appAttrMap = const theMap
    }
