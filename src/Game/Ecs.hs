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
import Data.Message qualified as Message
import Data.Monoid
import Data.Name qualified as Name
import Data.Position (Position (..))
import Data.Position qualified as Position
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Dhall qualified
import Game.Action
import Data.Amount (Amount)
import Game.Callbacks
import Game.Canvas qualified as Canvas
import Game.Canvas qualified as Game (Canvas)
import Game.Command
import Game.Entity.Enemy qualified as Enemy
import Game.Entity.Player qualified as Player
import Data.Function
import Game.Info qualified as Game (Info)
import Game.Info qualified as Info
import Game.State qualified
import Game.World qualified as Game (World)
import Linear (V2 (..))
import Optics hiding (assign, use)
import Optics.Tupled
import TextShow

type GameState = Game.State.State

-- | Kick off the ECS with provided channels and inputs. If we get
-- more channels/mvars, we should pull those out into their own
-- record.
start :: BChan Command -> MVar Action -> Game.World -> IO ()
start cmds acts world = do
  let initialState = (Game.State.State (Apecs.Entity 0) True)
  values <- Dhall.inputFile Dhall.auto "cfg/enemy.dhall"
  void
    . forkIO
    . Random.runRandomSystem
    . runTrace
    . runReader cmds
    . runReader acts
    . runReader @(Vector Enemy.Enemy) values
    . evalState initialState
    . Apecs.runWith world
    $ setup *> forever loop

-- | Initial setup associated with ECS creation.
setup ::
  ( Has (State Game.State.State) sig m,
    Has (Reader (Vector Enemy.Enemy)) sig m,
    Has Random sig m,
    MonadIO m
  ) =>
  Apecs.SystemT Game.World m ()
setup = do
  Apecs.newEntity (Player.initial ^. tupled)
    >>= assign Game.State.player

  allEnemies <- ask @(Vector Enemy.Enemy)

  let mkEnemy e = do
        pos <- fix $ \f -> do
          pos <- Position.randomIn 1 10
          occ <- occupied pos
          if occ then f else pure pos

        Apecs.newEntity (e ^. tupled, HP 5 5, pos)

  Vector.mapM_ mkEnemy allEnemies

  for_ Canvas.borders \border -> do
    Apecs.newEntity (border, Glyph '#', Color.White, Invalid)

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
    Has (State GameState) sig m,
    Has Trace sig m
  ) =>
  Apecs.SystemT Game.World m ()
loop = do
  trace "Loopin"
  next <- ask >>= liftIO . takeMVar @Action
  debug <- use Game.State.debugMode

  case next of
    Move dir -> do
      adjusted <- (if not debug then offsetRandomly else pure) dir
      prospective <- Position.offset adjusted <$> playerPosition
      present <- occupant prospective
      case present of
        Nothing -> movePlayer dir
        Just ent -> collideWith ent
    NoOp -> pure ()

  canv <- draw
  newinfo <- currentInfo
  Channel.writeB (Update newinfo)
  Channel.writeB (Redraw canv)
  trace "Done"

  pure ()

collideWith ::
  (MonadIO m, Has (Reader (BChan Command)) sig m, Has Random sig m, Has (State GameState) sig m) =>
  Apecs.Entity ->
  Apecs.SystemT Game.World m ()
collideWith ent = do
  (cb, name, canDrop, pos, mValue) <- Apecs.get ent
  case cb of
    Attack -> do
      HP curr _ <- Apecs.get ent
      dam :: Int <- Random.uniformR (1, 5)
      Channel.writeB (Notify (Message.fromText ("You attack for " <> showt dam <> " damage.")))
      let new = fromIntegral curr - dam
      if new <= 0
        then do
          Channel.writeB (Notify (Message.fromText ("You kill the " <> Name.text name <> ".")))
          Apecs.destroy ent (Apecs.Proxy @Enemy.Impl)
          Apecs.destroy ent (Apecs.Proxy @(HP, Position))

          case canDrop :: Amount of
            0 -> pure ()
            n -> do
              amt <- Random.uniformR (1, n)
              void $ Apecs.newEntity (amt, pos :: Position, Glyph '$', Color.Brown, PickUp)

        else do
          Apecs.modify ent (\(HP c m) -> HP (c - fromIntegral) m)
    Invalid -> Channel.writeB (Notify "You can't go that way.")
    PickUp -> do
      case mValue :: Maybe Amount of
        Nothing -> pure ()
        Just x -> do
          play <- use Game.State.player
          Apecs.modify play (+ x)
          Channel.writeB (Notify (Message.fromText ("You pick up " <> showt x <> " gold.")))
          Apecs.destroy ent (Apecs.Proxy @(Amount, Position, Glyph, Color.Color, Collision))


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

  player <- use Game.State.player
  Apecs.modify player \(Position p) -> Position (offset + p)

currentInfo ::
  ( MonadIO m,
    Has (State GameState) sig m
  ) =>
  Apecs.SystemT Game.World m Game.Info
currentInfo = do
  (hp, gold) <- Apecs.get =<< use Game.State.player
  let info = mempty @Info.Info
        & Info.hitpoints ?~ hp
        & Info.gold .~ gold
  pure info

playerPosition :: (Has (State GameState) sig m, MonadIO m) => Apecs.SystemT Game.World m Position
playerPosition = Apecs.get =<< use Game.State.player

occupant :: MonadIO m => Position -> Apecs.SystemT Game.World m (Maybe Apecs.Entity)
occupant p = fmap snd . getAlt <$> cfoldMap go
  where
    go :: (Position, Apecs.Entity) -> Alt Maybe (Position, Apecs.Entity)
    go x = x <$ guard (fst x == p)

occupied :: MonadIO m => Position -> Apecs.SystemT Game.World m Bool
occupied = fmap isJust . occupant

cfoldMap :: forall w m c a. (Apecs.Members w m c, Apecs.Get w m c, Monoid a) => (c -> a) -> Apecs.SystemT w m a
cfoldMap f = Apecs.cfold (\a b -> a <> f b) mempty
