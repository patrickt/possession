{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Game.Ecs (start, cfoldMap) where

import Apecs qualified
import Brick.BChan
import Control.Algebra qualified as Eff
import Control.Carrier.Reader hiding (Has)
import Control.Carrier.State.Strict hiding (Has)
import Control.Carrier.Trace.Ignoring hiding (Has)
import Control.Concurrent
import Control.Effect.Optics
import Control.Monad
import Control.Monad.IO.Class
import Data.Generics.Product
import Data.Foldable (for_)
import Data.Maybe (isJust)
import Data.Monoid
import Game.Action
import Game.Canvas qualified as Canvas
import Game.Canvas qualified as Game (Canvas)
import Game.State qualified
import Game.World qualified as Game (World)
import Game.World qualified as World
import Linear (V2 (..))
import Relude.Bool.Guard

draw :: (Eff.Has Trace sig m, MonadIO m) => Apecs.SystemT Game.World m Game.Canvas
draw = do
  trace "Run::draw"
  new <- Apecs.cfold go []
  trace (show new)
  pure (Canvas.empty `Canvas.update` new)
  where
    go :: [(World.Position, Canvas.Sprite)] -> (World.Position, World.Glyph, World.Color) -> [(World.Position, Canvas.Sprite)]
    go acc (pos, chr, color) = (pos, Canvas.Sprite chr color) : acc

loop ::
  ( MonadIO m,
    Eff.Has (Reader (MVar Action)) sig m,
    Eff.Has (Reader (BChan Command)) sig m,
    Eff.Has (State Game.State.State) sig m,
    Eff.Has Trace sig m
  ) =>
  Apecs.SystemT Game.World m ()
loop = do
  trace "Loopin"
  next <- ask >>= liftIO . takeMVar @Action

  case next of
    Move dir -> do
      prospective <- (World.Position dir +) <$> playerPosition
      unlessM (occupied prospective) $
        movePlayer dir
    NoOp -> pure ()

  canv <- draw
  pipe <- ask
  liftIO (writeBChan pipe (Redraw canv))
  trace "Done"

  pure ()

movePlayer :: MonadIO m => V2 Int -> Apecs.SystemT Game.World m ()
movePlayer dx = Apecs.cmap \(World.Position p, World.Player) -> World.Position (dx + p)

playerPosition :: (Eff.Has (State Game.State.State) sig m, MonadIO m) => Apecs.SystemT Game.World m World.Position
playerPosition = do
  p <- use (field @"player" @Game.State.State)
  (World.Player, loc) <- Apecs.get p
  pure loc

occupied :: MonadIO m => World.Position -> Apecs.SystemT Game.World m Bool
occupied p = isJust . getAlt <$> cfoldMap go
  where
    go :: World.Position -> Alt Maybe World.Position
    go x = guard (x == p) *> pure x

cfoldMap :: forall w m c a. (Apecs.Members w m c, Apecs.Get w m c, Monoid a) => (c -> a) -> Apecs.SystemT w m a
cfoldMap f = Apecs.cfold (\a b -> a <> f b) (mempty :: a)

setup :: (Eff.Has (State Game.State.State) sig m, MonadIO m) => Apecs.SystemT Game.World m ()
setup = do
  Apecs.newEntity (World.Position 3, World.Player, World.Glyph '@', World.White)
    >>= assign (field @"player" @Game.State.State)

  for_ Canvas.borders \border -> do
    Apecs.newEntity (border, World.Wall, World.Glyph '#', World.White)

start :: BChan Command -> MVar Action -> Game.World -> IO ()
start cmds acts world =
  void
    . forkIO
    . runTrace
    . runReader cmds
    . runReader acts
    . evalState (Game.State.State (error "BUG: Tried to read uninitialized player"))
    . Apecs.runWith world
    $ setup *> forever loop
