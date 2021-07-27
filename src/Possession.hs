{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}

module Possession (main) where

import Brick qualified
import Brick.BChan
import Control.Concurrent.STM.TBQueue
import Control.Monad
import Game.Ecs qualified as Ecs
import Optics
import Game.World qualified as Game
import Graphics.Vty qualified as Vty
import UI.App qualified as App
import UI.State qualified
import Control.Effect.Broker (Brokerage(Brokerage))

main :: IO ()
main = do
  broker <- Brokerage <$> newBChan 1 <*> newTBQueueIO 100
  vty <- Vty.standardIOConfig >>= Vty.mkVty
  w <- Game.initWorld
  thread <- Ecs.start broker w
  let ui = UI.State.initial broker thread
  void $ Brick.customMain vty (pure vty) (broker ^? #brickQueue) App.app ui
