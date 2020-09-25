{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

module Game.Run (start) where

import Apecs qualified
import Brick.BChan
import Control.Algebra qualified as Eff
import Control.Carrier.Reader hiding (Has)
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Game.Action
import Game.World qualified as Game (World)

type Command = ()

loop ::
  ( MonadIO m,
    Eff.Has (Reader (MVar Action)) sig m
  ) =>
  Apecs.SystemT Game.World m ()
loop = do
  _next <- ask >>= liftIO . takeMVar @Action
  pure ()

start :: BChan Command -> MVar Action -> Game.World -> IO ()
start cmds acts world =
  void
    . forkIO
    . runReader cmds
    . runReader acts
    . flip Apecs.runSystem world
    . forever
    $ loop
