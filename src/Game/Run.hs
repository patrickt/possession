{-# LANGUAGE BlockArguments #-}
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
import Game.World qualified as World
import Linear
import Game.Canvas qualified as Game (Canvas)
import Game.Canvas qualified as Canvas

draw :: MonadIO m => Apecs.SystemT Game.World m Game.Canvas
draw = do
  new <- Apecs.cfold go []
  pure (Canvas.empty `Canvas.update` new)
  where
    go :: [(World.Position, Canvas.Sprite)] -> (World.Position, World.Glyph, World.Color) -> [(World.Position, Canvas.Sprite)]
    go acc (pos, chr, color) = (pos, Canvas.Sprite chr color) : acc

loop ::
  ( MonadIO m,
    Eff.Has (Reader (MVar Action)) sig m,
    Eff.Has (Reader (BChan Command)) sig m
  ) =>
  Apecs.SystemT Game.World m ()
loop = do
  next <- ask >>= liftIO . takeMVar @Action

  case next of
    Move dir -> movePlayer dir


  canv <- draw
  pipe <- ask
  liftIO (writeBChan pipe (Redraw canv))

  pure ()

movePlayer :: MonadIO m => V2 Int -> Apecs.SystemT Game.World m ()
movePlayer dx = Apecs.cmap \(World.Position p, World.Player) -> World.Position (dx + p)

setup :: MonadIO m => Apecs.SystemT Game.World m ()
setup = do
  void $ Apecs.newEntity (World.Position 3, World.Player, World.Glyph '@')
  void $ Apecs.newEntity (World.Position 1, World.Wall, World.Glyph '#')

start :: BChan Command -> MVar Action -> Game.World -> IO ()
start cmds acts world =
  void
    . forkIO
    . runReader cmds
    . runReader acts
    . Apecs.runWith world
    $ setup *> forever loop
