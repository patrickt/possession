{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

module Game.Run (start) where

import Brick.BChan
import Control.Algebra qualified as Eff
import Control.Carrier.Reader hiding (Has)
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Game.Action

type Command = ()

loop ::
  (MonadIO m, Eff.Has (Reader (MVar Action)) sig m) =>
  m ()
loop = do
  _next <- ask >>= liftIO . takeMVar @Action
  pure ()

start :: BChan Command -> MVar Action -> IO ()
start cmds acts =
  void
    . forkIO
    . runReader cmds
    . runReader acts
    . forever
    $ loop
