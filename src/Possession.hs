module Possession (main) where

import Brick qualified
import Brick.BChan (newBChan)
import Control.Concurrent.STM.TBQueue (newTBQueueIO)
import Control.Effect.Broker (Brokerage (Brokerage))
import Control.Monad (void)
import Game.Ecs qualified as Ecs
import Game.World qualified as Game
import Graphics.Vty qualified as Vty
import Optics ((^?))
import UI.App qualified as App
import UI.State qualified

main :: IO ()
main = do
  broker <- Brokerage <$> newBChan 1 <*> newTBQueueIO 100
  vty <- Vty.standardIOConfig >>= Vty.mkVty
  thread <- Game.initWorld >>= Ecs.start broker
  let ui = UI.State.initial broker thread
  void $ Brick.customMain vty (pure vty) (broker ^? #brickQueue) App.app ui
