{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Game.Ecs (start, cfoldMap) where

import Apecs qualified
import Brick.BChan
import Control.Carrier.Random.Lifted qualified as Random
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Control.Carrier.Trace.Ignoring
import Control.Concurrent
import Control.Effect.Optics
import Control.Effect.Random (Random)
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable (for_)
import Data.Maybe (isJust)
import Data.Monoid
import Data.Position (Position (..))
import Data.Position qualified as Position
import Game.Action
import Game.Canvas qualified as Canvas
import Game.Canvas qualified as Game (Canvas)
import Game.State qualified
import Game.World qualified as Game (World)
import Game.World qualified as World
import Linear (V2 (..))
import Relude.Bool.Guard

type GameState = Game.State.State

draw :: (Has Trace sig m, MonadIO m) => Apecs.SystemT Game.World m Game.Canvas
draw = do
  trace "Run::draw"
  new <- Apecs.cfold go []
  trace (show new)
  pure (Canvas.empty `Canvas.update` new)
  where
    go :: [(Position, Canvas.Sprite)] -> (Position, World.Glyph, World.Color) -> [(Position, Canvas.Sprite)]
    go acc (pos, chr, color) = (pos, Canvas.Sprite chr color) : acc

loop ::
  ( MonadIO m,
    Has Random sig m,
    Has (Reader (MVar Action)) sig m,
    Has (Reader (BChan Command)) sig m,
    Has (State Game.State.State) sig m,
    Has Trace sig m
  ) =>
  Apecs.SystemT Game.World m ()
loop = do
  trace "Loopin"
  next <- ask >>= liftIO . takeMVar @Action
  debug <- use Game.State.debugMode

  case next of
    Move dir -> do
      adjusted <- (if debug then offsetRandomly else pure) dir
      prospective <- Position.offset adjusted <$> playerPosition
      unlessM (occupied prospective) $
        movePlayer dir
    NoOp -> pure ()

  canv <- draw
  pipe <- ask
  liftIO (writeBChan pipe (Redraw canv))
  trace "Done"

  pure ()

offsetRandomly :: Has Random sig m => V2 Int -> m (V2 Int)
offsetRandomly (V2 x y) = V2 <$> go x <*> go y
  where
    go v = do
      fuzz <- Random.uniformR (0, 2)
      degree <- Random.uniformR (1, 3)
      pure ((v + fuzz) * degree)

movePlayer ::
  ( Has (State GameState) sig m,
    Has Random sig m,
    MonadIO m
  ) =>
  V2 Int ->
  Apecs.SystemT Game.World m ()
movePlayer dx = do
  debug <- use Game.State.debugMode
  offset <- (if debug then pure else offsetRandomly) dx

  Apecs.cmap \(Position p, World.Player) -> Position (offset + p)

playerPosition :: (Has (State GameState) sig m, MonadIO m) => Apecs.SystemT Game.World m Position
playerPosition = do
  (World.Player, loc) <- Apecs.get =<< use Game.State.player
  pure loc

occupied :: MonadIO m => Position -> Apecs.SystemT Game.World m Bool
occupied p = isJust . getAlt <$> cfoldMap go
  where
    go :: Position -> Alt Maybe Position
    go x = x <$ guard (x == p)

cfoldMap :: forall w m c a. (Apecs.Members w m c, Apecs.Get w m c, Monoid a) => (c -> a) -> Apecs.SystemT w m a
cfoldMap f = Apecs.cfold (\a b -> a <> f b) mempty

setup :: (Has (State Game.State.State) sig m, MonadIO m) => Apecs.SystemT Game.World m ()
setup = do
  Apecs.newEntity (Position 3, World.Player, World.Glyph '@', World.White)
    >>= assign Game.State.player

  for_ Canvas.borders \border -> do
    Apecs.newEntity (border, World.Wall, World.Glyph '#', World.White)

start :: BChan Command -> MVar Action -> Game.World -> IO ()
start cmds acts world =
  let initialState = (Game.State.State (Apecs.Entity 0) False)
   in void
        . forkIO
        . Random.runRandomSystem
        . runTrace
        . runReader cmds
        . runReader acts
        . evalState initialState
        . Apecs.runWith world
        $ setup *> forever loop
