{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Provides the high-level constructs associated with the
-- game thread. Receives 'Action' values from the UI and sends
-- 'Command' values back.
module Game.Ecs (start) where

import Apecs (Entity)
import Apecs.Exts qualified as Apecs
import Control.Carrier.Random.Lifted qualified as Random
import Control.Carrier.Reader (Has, Reader, ask, runReader)
import Control.Carrier.State.Strict (State, evalState)
import Control.Carrier.Trace.Ignoring (Trace, runTrace, trace)
import Control.Concurrent (ThreadId, forkIO)
import Control.Effect.Broker
  ( Broker,
    Brokerage,
    notify,
    runBroker,
  )
import Control.Effect.Broker qualified as Broker
import Control.Effect.Optics (use)
import Control.Effect.Random (Random)
import Control.Monad (forever, guard, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Amount (Amount)
import Data.Color qualified as Color
import Data.Experience (XP (..))
import Data.Foldable (for_)
import Data.Foldable.WithIndex (iforM_)
import Data.Function ((&))
import Data.Glyph (Glyph (..))
import Data.Hitpoints as HP (HP, injure, isDead)
import Data.Maybe (isJust)
import Data.Message qualified as Message
import Data.Monoid (Alt (getAlt), Last)
import Data.Name (Name)
import Data.Name qualified as Name
import Data.Position (Position, position)
import System.Random.MWC qualified as MWC
import Data.Position qualified as Position
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Dhall qualified
import Game.Action
  ( Action (LoadState, Move, Redraw, SaveState, Start, Update),
  )
import Game.Behavior (Collision (..))
import Game.Canvas qualified as Canvas
import Game.Canvas qualified as Game (Canvas)
import Game.Dungeon qualified as Dungeon
import Raw.Enemy qualified as Raw
import Game.Entity.Enemy qualified as Enemy
import Game.Entity.Player qualified as Player
import Game.Entity.Terrain qualified as Terrain
import Game.Info qualified as Game (Info)
import Game.Save qualified as Save
import Game.State qualified
import Game.World qualified as Game (World)
import Linear (V2 (..))
import Optics (At (at), (%), (.~), (?~))
import TextShow (TextShow (showt))

type GameState = Game.State.State

runRandomFaster :: MonadIO m => MWC.GenIO -> Random.RandomC IO m a -> m a
runRandomFaster sr (Random.RandomC act) = runReader sr act
{-# INLINE runRandomFaster #-}

-- | Kick off the ECS with provided channels and inputs. If we get
-- more channels/mvars, we should pull those out into their own
-- record.
start :: Brokerage -> Game.World -> IO ThreadId
start broker world = do
  values <- Dhall.inputFile Dhall.auto "cfg/enemy.dhall"
  rand <- MWC.createSystemRandom
  forkIO
    . runRandomFaster rand
    . runTrace
    . runReader @(Vector Raw.Enemy) values
    . runBroker broker
    . evalState Game.State.initial
    . Apecs.runWith world
    $ setup *> loop

-- | Initial setup associated with ECS creation.
setup ::
  ( Has (State Game.State.State) sig m,
    Has (Reader (Vector Raw.Enemy)) sig m,
    Has Random sig m,
    MonadIO m
  ) =>
  Apecs.SystemT Game.World m ()
setup = do
  -- Build some walls
  for_ Canvas.borders (Apecs.newEntity_ . Terrain.wall)

  map' <- liftIO Dungeon.makeDungeon
  iforM_ map' $ \pos cell ->
    when (cell == Dungeon.On) $
      Apecs.newEntity_ (Terrain.wall pos)

  -- Create the player
  playerStart <- findUnoccupied
  Apecs.set player (Player.initial & position .~ playerStart)

  -- Fill in some enemies
  let mkEnemy (e :: Raw.Enemy) = do
        pos <- findUnoccupied
        Apecs.newEntity (Enemy.fromRaw pos e)

  ask @(Vector Raw.Enemy) >>= Vector.mapM_ mkEnemy

findUnoccupied :: (MonadIO m, Has Random sig m) => Apecs.SystemT Game.World m Position
findUnoccupied = do
  pos <- Position.randomIn 1 50
  occ <- occupied pos
  if occ then findUnoccupied else pure pos

-- | Renders a canvas from the current system, for passing back to the display thread.
draw :: (Has Trace sig m, MonadIO m) => Apecs.SystemT Game.World m Game.Canvas
draw = Apecs.cfold go Canvas.empty
  where
    go c (pos, chr, color) = Canvas.update c pos (Canvas.Sprite chr color)

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
      adjusted <- (if not debug then Position.offsetRandomly else pure) dir
      prospective <- (adjusted +) <$> playerPosition
      present <- occupant prospective
      maybe (movePlayer dir) collideWith present
    SaveState -> do
      Save.save >>= Save.write
      Broker.notify "Game saved."
    LoadState -> do
      Save.read >>= Save.load
      Broker.notify "Game loaded."
    Start -> pure ()

  canv <- draw
  newinfo <- currentInfo
  Broker.sendCommand (Update newinfo)
  Broker.sendCommand (Redraw canv)
  trace "Done"

playerAttack :: (MonadIO m, Has Broker sig m, Has Random sig m, Has (State GameState) sig m) => Entity -> Apecs.SystemT Game.World m ()
playerAttack ent = do
  (hp, pos :: Position, canDrop :: Amount, xp :: XP, name) <- Apecs.get ent
  dam :: Int <- Random.uniformR (1, 5)
  Broker.notify (Message.fromText ("You attack for " <> showt dam <> " damage."))
  let new = HP.injure dam hp
  if HP.isDead new
    then do
      notify (Message.fromText ("You kill the " <> Name.text name <> "."))
      Apecs.remove @Enemy.Enemy ent

      Apecs.append player xp

      when (canDrop /= 0) do
        amt <- Random.uniformR (1, canDrop)
        Apecs.newEntity_ (amt, pos :: Position, Glyph '$', Color.Brown, PickUp)
    else Apecs.set ent new

playerPickUp :: (Has (State GameState) sig m, Has Broker sig m, MonadIO m) => Entity -> Apecs.SystemT Game.World m ()
playerPickUp ent = do
  (mValue, pos :: Position) <- Apecs.get ent
  case mValue :: Maybe Amount of
    Nothing -> pure ()
    Just x -> do
      Apecs.modify player (+ x)
      Broker.notify (Message.fromText ("You pick up " <> showt x <> " gold."))
      Apecs.destroy ent (Apecs.Proxy @(Amount, Position, Glyph, Color.Color, Collision))
      Apecs.set player pos

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

movePlayer ::
  ( Has (State GameState) sig m,
    Has Random sig m,
    MonadIO m
  ) =>
  V2 Int ->
  Apecs.SystemT Game.World m ()
movePlayer dx = do
  debug <- use @GameState #debugMode
  offset <- (if debug then pure else Position.offsetRandomly) dx

  Apecs.modify player (offset +)

currentInfo ::
  ( MonadIO m,
    Has (State GameState) sig m
  ) =>
  Apecs.SystemT Game.World m Game.Info
currentInfo = do
  (hp :: HP, gold, xp, pos) <- Apecs.get player
  -- use the info as the accumulator itself
  -- when there's a Selection present, emit something other than mempty
  let info =
        mempty
          & #hitpoints .~ pure @Last hp
          & #gold .~ pure gold
          & #xp .~ xp
          & #position .~ pure @Last @Position pos

  let go inf (name :: Name, p :: Position) = inf & #summary % at p ?~ name

  Apecs.cfold go info

player :: Entity
player = Apecs.global

playerPosition :: (Has (State GameState) sig m, MonadIO m) => Apecs.SystemT Game.World m Position
playerPosition = Apecs.get player

occupant :: MonadIO m => Position -> Apecs.SystemT Game.World m (Maybe Apecs.Entity)
occupant p = fmap snd . getAlt <$> Apecs.cfoldMap go
  where
    go :: (Position, Apecs.Entity) -> Alt Maybe (Position, Apecs.Entity)
    go x = x <$ guard (fst x == p)

occupied :: MonadIO m => Position -> Apecs.SystemT Game.World m Bool
occupied = fmap isJust . occupant
