{-# LANGUAGE ImportQualifiedPost #-}

module Possession (main) where

import Brick qualified
import Brick.BChan
import Control.Concurrent.STM.TBQueue
import Control.Monad
import Game.Ecs qualified as Ecs
import Game.World qualified as Game
import Graphics.Vty qualified as Vty
import UI.App qualified as App
import UI.State qualified

main :: IO ()
main = do
  brickEvLoop <- newBChan 1
  actionBox <- newTBQueueIO 100
  vty <- Vty.standardIOConfig >>= Vty.mkVty
  w <- Game.initWorld
  thread <- Ecs.start brickEvLoop actionBox w
  let ui = UI.State.initial actionBox thread
  void $ Brick.customMain vty (pure vty) (Just brickEvLoop) App.app ui
