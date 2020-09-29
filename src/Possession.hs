{-# LANGUAGE ImportQualifiedPost #-}

module Possession (main) where

import Brick qualified
import Brick.BChan
import Control.Concurrent.MVar
import Control.Monad
import Game.Run qualified as Game
import Game.World qualified as Game
import Graphics.Vty qualified as Vty
import UI.App qualified as App
import UI.State qualified

main :: IO ()
main = do
  world <- Game.initWorld
  brickEvLoop <- newBChan 10
  actionBox <- newEmptyMVar
  vty <- Vty.standardIOConfig >>= Vty.mkVty
  Game.start brickEvLoop actionBox world
  let ui = UI.State.initial actionBox
  void $ Brick.customMain vty (pure vty) (Just brickEvLoop) App.app ui
