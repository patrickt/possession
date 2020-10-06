{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Control.Effect.Channel qualified as Channel
import Control.Effect.Optics
import Control.Effect.Random (Random)
import Control.Monad
import Control.Monad.IO.Class
import Data.Color qualified as Color
import Data.Foldable (for_)
import Data.Glyph
import Data.Hitpoints
import Data.Maybe (isJust)
import Data.Monoid
import Data.Position (Position (..))
import Data.Position qualified as Position
import Game.Action
import Game.Canvas qualified as Canvas
import Game.Canvas qualified as Game (Canvas)
import Game.Command
import Game.Entity.Player qualified as Player
import Game.Info qualified as Game (Info)
import Game.Info qualified as Info
import Game.State qualified
import Game.World qualified as Game (World)
import Game.World qualified as World
import Linear (V2 (..))
import Optics.Operators.Unsafe

type GameState = Game.State.State

-- | Kick off the ECS with provided channels and inputs. If we get
-- more channels/mvars, we should pull those out into their own
-- record.
start :: BChan Command -> MVar Action -> Game.World -> IO ()
start cmds acts world =
  let initialState = (Game.State.State (Apecs.Entity 0) True)
   in void
        . forkIO
        . Random.runRandomSystem
        . runTrace
        . runReader cmds
        . runReader acts
        . evalState initialState
        . Apecs.runWith world
        $ setup *> forever loop

-- | Initial setup associated with ECS creation.
setup :: (Has (State Game.State.State) sig m, MonadIO m) => Apecs.SystemT Game.World m ()
setup = do
  Apecs.newEntity (Player.initial ^?! Player._Player)
    >>= assign Game.State.player

  for_ Canvas.borders \border -> do
    Apecs.newEntity (border, World.Wall, Glyph '#', Color.White)

draw :: (Has Trace sig m, MonadIO m) => Apecs.SystemT Game.World m Game.Canvas
draw = do
  trace "Run::draw"
  new <- Apecs.cfold go []
  trace (show new)
  pure (Canvas.empty `Canvas.update` new)
  where
    go :: [(Position, Canvas.Sprite)] -> (Position, Glyph, Color.Color) -> [(Position, Canvas.Sprite)]
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
  pipe <- ask

  case next of
    Move dir -> do
      adjusted <- (if not debug then offsetRandomly else pure) dir
      prospective <- Position.offset adjusted <$> playerPosition
      invalid <- occupied prospective
      if invalid
        then liftIO (writeBChan pipe (Notify "You can't go that way."))
        else movePlayer dir
    NoOp -> pure ()

  canv <- draw
  newinfo <- currentInfo
  Channel.writeB (Update newinfo)
  Channel.writeB (Redraw canv)
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

  Apecs.cmap \(Position p, Player.Self) -> Position (offset + p)

currentInfo ::
  ( MonadIO m,
    Has (State GameState) sig m
  ) =>
  Apecs.SystemT Game.World m Game.Info
currentInfo = do
  (Player.Self, hp) <- Apecs.get =<< use Game.State.player
  pure Info.Info {Info.playerHitpoints = Last (Just hp)}

playerPosition :: (Has (State GameState) sig m, MonadIO m) => Apecs.SystemT Game.World m Position
playerPosition = do
  (Player.Self, loc) <- Apecs.get =<< use Game.State.player
  pure loc

occupied :: MonadIO m => Position -> Apecs.SystemT Game.World m Bool
occupied p = isJust . getAlt <$> cfoldMap go
  where
    go :: Position -> Alt Maybe Position
    go x = x <$ guard (x == p)

cfoldMap :: forall w m c a. (Apecs.Members w m c, Apecs.Get w m c, Monoid a) => (c -> a) -> Apecs.SystemT w m a
cfoldMap f = Apecs.cfold (\a b -> a <> f b) mempty
