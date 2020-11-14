{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

-- | Provides the high-level constructs associated with the
-- game thread. Receives 'Action' values from the UI and sends
-- 'Command' values back.
module Game.Ecs (start, cfoldMap) where

import Apecs (Entity)
import Apecs qualified
import Brick.BChan
import Control.Carrier.Random.Lifted qualified as Random
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Control.Carrier.Trace.Ignoring
import Control.Concurrent
import Control.Effect.Broker
import Control.Effect.Broker qualified as Broker
import Control.Effect.Optics
import Control.Effect.Random (Random)
import Control.Monad
import Control.Monad.IO.Class
import Data.Amount (Amount)
import Data.Color qualified as Color
import Data.Experience (XP (..))
import Data.Foldable (for_)
import Data.Function
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
import Game.Behavior
import Game.Canvas qualified as Canvas
import Game.Canvas qualified as Game (Canvas)
import Game.Entity.Enemy qualified as Enemy
import Game.Entity.Player qualified as Player
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
start :: BChan (Action 'UI) -> MVar (Action 'Game) -> Game.World -> IO ThreadId
start cmds acts world = do
  let initialState = (Game.State.State (Apecs.Entity 0) True)
  values <- Dhall.inputFile Dhall.auto "cfg/enemy.dhall"
  forkIO
    . Random.runRandomSystem
    . runTrace
    . runReader @(Vector Enemy.Enemy) values
    . runBroker cmds acts
    . evalState initialState
    . Apecs.runWith world
    $ setup *> loop

-- | Initial setup associated with ECS creation.
setup ::
  ( Has (State Game.State.State) sig m,
    Has (Reader (Vector Enemy.Enemy)) sig m,
    Has Random sig m,
    MonadIO m
  ) =>
  Apecs.SystemT Game.World m ()
setup = do
  -- Create the player
  Apecs.newEntity (Player.initial ^. tupled)
    >>= assign @GameState #player

  -- Fill in some enemies
  let mkEnemy e = do
        -- This function could be pulled out into
        -- findUnoccupied :: m Position
        pos <- fix $ \f -> do
          pos <- Position.randomIn 1 10
          occ <- occupied pos
          if occ then f else pure pos

        Apecs.newEntity (e ^. tupled, HP 5 5, pos)

  ask @(Vector Enemy.Enemy) >>= Vector.mapM_ mkEnemy

  -- Build some walls
  for_ Canvas.borders \border -> do
    Apecs.newEntity (border, Glyph '#', Color.White, Invalid)

-- | Renders a canvas from the current system, for passing back to the display thread.
draw :: (Has Trace sig m, MonadIO m) => Apecs.SystemT Game.World m Game.Canvas
draw = Apecs.cfold (flip go) Canvas.empty
  where
    go (pos, chr, color) = flip Canvas.update [(pos, Canvas.Sprite chr color)]

-- The main game loop.
loop ::
  ( MonadIO m,
    Has Random sig m,
    Has Broker sig m,
    Has (State GameState) sig m,
    Has Trace sig m
  ) =>
  Apecs.SystemT Game.World m ()
loop = forever do
  next <- Broker.popAction
  debug <- use @GameState #debugMode

  case next of
    Move dir -> do
      adjusted <- (if not debug then offsetRandomly else pure) dir
      prospective <- Position.offset adjusted <$> playerPosition
      present <- occupant prospective
      maybe (movePlayer dir) collideWith present

    NoOp -> pure ()
    Exit -> pure ()

  canv <- draw
  newinfo <- currentInfo
  Broker.sendCommand (Update newinfo)
  Broker.sendCommand (Redraw canv)
  trace "Done"

playerAttack :: (MonadIO m, Has Broker sig m, Has Random sig m, Has (State GameState) sig m) => Entity -> Apecs.SystemT Game.World m ()
playerAttack ent = do
  (HP curr _, pos :: Position, canDrop, mXP, name) <- Apecs.get ent
  play <- use @GameState #player
  dam :: Int <- Random.uniformR (1, 5)
  Broker.notify (Message.fromText ("You attack for " <> showt dam <> " damage."))
  let new = fromIntegral curr - dam
  if new <= 0
    then do
      notify (Message.fromText ("You kill the " <> Name.text name <> "."))
      Apecs.destroy ent (Apecs.Proxy @Enemy.Impl)
      Apecs.destroy ent (Apecs.Proxy @(HP, Position))

      case mXP :: Maybe XP of
        Nothing -> pure ()
        Just x -> Apecs.modify play (<> x)

      case canDrop :: Amount of
        0 -> pure ()
        n -> do
          amt <- Random.uniformR (1, n)
          void $ Apecs.newEntity (amt, pos :: Position, Glyph '$', Color.Brown, PickUp)
    else do
      Apecs.modify ent (\(HP c m) -> HP (c - fromIntegral dam) m)

playerPickUp :: (Has (State GameState) sig m, Has Broker sig m, MonadIO m) => Entity -> Apecs.SystemT Game.World m ()
playerPickUp ent = do
  (mValue, pos :: Position) <- Apecs.get ent
  play <- use @GameState #player
  case mValue :: Maybe Amount of
    Nothing -> pure ()
    Just x -> do
      Apecs.modify play (+ x)
      Broker.notify (Message.fromText ("You pick up " <> showt x <> " gold."))
      Apecs.destroy ent (Apecs.Proxy @(Amount, Position, Glyph, Color.Color, Collision))
      Apecs.set play pos

collideWith ::
  (MonadIO m, Has Broker sig m, Has Random sig m, Has (State GameState) sig m) =>
  Apecs.Entity ->
  Apecs.SystemT Game.World m ()
collideWith ent = do
  cb <- Apecs.get ent
  case cb of
    Attack -> playerAttack ent
    Invalid -> Broker.notify "You can't go that way."
    PickUp -> playerPickUp ent

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
  debug <- use @GameState #debugMode
  offset <- (if debug then pure else offsetRandomly) dx

  player <- use @GameState #player
  Apecs.modify player \(Position p) -> Position (offset + p)

currentInfo ::
  ( MonadIO m,
    Has (State GameState) sig m
  ) =>
  Apecs.SystemT Game.World m Game.Info
currentInfo = do
  (hp :: HP, gold, xp, pos) <- Apecs.get =<< use @GameState #player
  -- use the info as the accumulator itself
  -- when there's a Selection present, emit something other than mempty
  let info =
        mempty @Info.Info
        & #hitpoints .~ pure @Last hp
        & #gold .~ pure gold
        & #xp .~ xp
        & #position .~ pure @Last @Position pos
        & pure

  info

playerPosition :: (Has (State GameState) sig m, MonadIO m) => Apecs.SystemT Game.World m Position
playerPosition = Apecs.get =<< use @GameState #player

occupant :: MonadIO m => Position -> Apecs.SystemT Game.World m (Maybe Apecs.Entity)
occupant p = fmap snd . getAlt <$> cfoldMap go
  where
    go :: (Position, Apecs.Entity) -> Alt Maybe (Position, Apecs.Entity)
    go x = x <$ guard (fst x == p)

occupied :: MonadIO m => Position -> Apecs.SystemT Game.World m Bool
occupied = fmap isJust . occupant

cfoldMap :: forall w m c a. (Apecs.Members w m c, Apecs.Get w m c, Monoid a) => (c -> a) -> Apecs.SystemT w m a
cfoldMap f = Apecs.cfold (\a b -> a <> f b) mempty
