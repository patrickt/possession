{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

-- | Provides the high-level constructs associated with the
-- game thread. Receives 'Action' values from the UI and sends
-- 'Command' values back.
module Game.Ecs (start) where

import Apecs (Entity)
import Apecs.Exts qualified as Apecs
import Control.Carrier.Random.Lifted qualified as Random
import Control.Carrier.Reader (runReader)
import Control.Carrier.State.Strict (State, evalState)
import Control.Carrier.Trace.Brokered
import Control.Concurrent (ThreadId, forkIO)
import Control.Effect.Broker
  ( Broker,
    Brokerage,
    notify,
    runBroker,
  )
import Control.Effect.Broker qualified as Broker
import Control.Effect.Optics
import Control.Effect.Random (Random)
import Control.Monad (forever, guard, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Amount
import Data.Experience (XP (..))
import Data.Foldable (for_)
import Data.Foldable.WithIndex (iforM_)
import Data.Glyph (Glyph (..))
import Data.Hitpoints as HP (HP, injure, isAlive)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Message qualified as Message
import Data.Monoid (Alt (getAlt))
import Data.Name qualified as Name
import Data.Position (Position, position)
import Data.Position qualified as Position
import Game.Action (Action (..))
import Game.Canvas qualified as Canvas
import Game.Canvas qualified as Game (Canvas)
import Game.Dungeon hiding (at)
import Game.Dungeon qualified as Dungeon hiding (at)
import Game.Entity.Enemy (_Enemy)
import Game.Entity.Enemy qualified as Enemy
import Game.Entity.Player qualified as Player
import Game.Entity.Terrain qualified as Terrain
import Game.Flag qualified as Flag
import Game.Info qualified as Game (Info)
import Game.Pathfind qualified as PF
import Game.Save qualified as Save
import Game.State qualified
import Game.World (WorldT)
import Game.World qualified as Game (World)
import Linear (V2 (..))
import Optics hiding (modifying, view, use, assign)
import Raw.Types (Collision (..), Strategy (..))
import Raw.Types qualified as Color (Color (..))
import Raw.Types qualified as Raw
import Raws (Raws)
import Raws qualified
import System.Random.MWC qualified as MWC
import TextShow (TextShow (showt))
import qualified Game.Info as Info
import Game.Info (HasInfo (info))
import Data.Generics.Product (typed)

type GameState = Game.State.State

runRandomFaster :: MonadIO m => MWC.GenIO -> Random.RandomC IO m a -> m a
runRandomFaster sr (Random.RandomC act) = runReader sr act
{-# INLINE runRandomFaster #-}

-- | Kick off the ECS with provided channels and inputs. If we get
-- more channels/mvars, we should pull those out into their own
-- record.
start :: Brokerage -> Game.World -> IO ThreadId
start broker world = do
  raws <- Raws.loadFromDhall
  rand <- MWC.createSystemRandom
  forkIO
    . runRandomFaster rand
    . evalState @Raws raws
    . runBroker broker
    . runTrace
    . evalState Game.State.initial
    . Apecs.runWith world
    $ setup *> loop

-- | Initial setup associated with ECS creation.
setup ::
  ( Has (State Game.State.State) sig m,
    Has (State Raws) sig m,
    Has Trace sig m,
    Has Random sig m,
    MonadIO m
  ) =>
  WorldT m ()
setup = do
  -- Build some walls
  for_ Canvas.borders (Apecs.newEntity_ . Terrain.wall)

  map' <- liftIO Dungeon.makeDungeon
  iforM_ (getDungeon map') $ \pos cell ->
    when (cell == Dungeon.On) $
      Apecs.newEntity_ (Terrain.wall pos)

  -- Create the player
  playerStart <- findUnoccupied
  Apecs.set player (Player.initial & position .~ playerStart)

  -- Fill in some enemies
  let mkEnemy idx (e :: Raw.Enemy) = do
        pos <- findUnoccupied
        Apecs.newEntity (re _Enemy # Enemy.fromRaw pos (Raw.Id idx) e)

  foes <- use @Raws #enemies
  itraverse_ mkEnemy foes
  recalculateInfo >>= assign (info @GameState)


enemyTurn :: (Has Broker sig m, MonadIO m, Has Trace sig m) => WorldT m ()
enemyTurn = do
  pp <- playerPosition

  let empties = Map.fromList ((,Dungeon.On) <$> Canvas.borders)
  impassables <- Apecs.cfoldMap (\(Flag.Impassable, p :: Position) -> Map.singleton p Dungeon.Off)
  let lookupTable = Map.union impassables empties
  let isEmpty x = Map.lookup x lookupTable == Just Dungeon.Off
  let neighborsOf p = filter isEmpty (Position.adjacentClamped Canvas.size p)

  Apecs.cmapM_ \(Enemy.EnemyTag, e :: Entity, pos :: Position, Hearing hearingRadius, strat :: Strategy) ->
    when (Position.dist pos pp < fromIntegral hearingRadius && strat == FightOnSight) do
      let pfctx = PF.Context pos pp (Position.dist pos) (\_ _ -> 1) neighborsOf
      let path = PF.pathfind' pfctx
      case path of
        (_ : y : _) -> do
          Apecs.set e y

          Apecs.set e Flag.Dirty
        _ -> pure ()

findUnoccupied :: (MonadIO m, Has Random sig m) => WorldT m Position
findUnoccupied = do
  pos <- Position.randomIn 1 50
  occ <- occupied pos
  if occ then findUnoccupied else pure pos

-- | Renders a canvas from the current system, for passing back to the display thread.
draw :: (Has Trace sig m, MonadIO m) => WorldT m Game.Canvas
draw = Apecs.cfold go Canvas.empty
  where
    go c (pos, chr, color) = Canvas.update c pos (Canvas.Sprite chr color Color.Black)

-- The main game loop.
loop ::
  ( MonadIO m,
    Has Random sig m,
    Has Broker sig m,
    Has (State GameState) sig m,
    Has (State Raws) sig m,
    Has Trace sig m
  ) =>
  WorldT m ()
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
    Terminate -> Broker.sendCommand Terminate
    Notify a -> Broker.sendCommand (Notify a)
    Start -> pure ()

  enemyTurn

  refreshInfo
  canv <- draw

  view (info @GameState) >>= Broker.sendCommand . Update
  Broker.sendCommand (Redraw canv)

refreshInfo :: (MonadIO m, Has (State GameState) sig m) => WorldT m ()
refreshInfo = do
  (dirties :: [(Flag.Dirty, (Position, Entity))]) <- Apecs.cfold (flip (:)) []
  let newInfo = mempty & Info.atlas .~ Map.fromList (fmap snd dirties)
  modifying @_ @GameState info (<> newInfo)

playerAttack :: (MonadIO m, Has Broker sig m, Has Random sig m, Has (State GameState) sig m) => Entity -> WorldT m ()
playerAttack ent = do
  (hp, pos :: Position, canDrop :: Amount, xp :: XP, name) <- Apecs.get ent
  dam :: Int <- Random.uniformR (1, 5)
  Broker.notify (Message.fromText ("You attack for " <> showt dam <> " damage."))
  let new = HP.injure dam hp
  if HP.isAlive new
    then Apecs.set ent new
    else do
      notify (Message.fromText ("You kill the " <> Name.text name <> "."))
      Apecs.removing _Enemy ent

      Apecs.append player xp

      when (canDrop /= 0) do
        amt <- Random.uniformR (1, canDrop)
        Apecs.newEntity_ (amt, pos :: Position, Glyph '$', Color.Brown, PickUp)

playerPickUp :: (Has (State GameState) sig m, Has Broker sig m, MonadIO m) => Entity -> WorldT m ()
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
  WorldT m ()
collideWith ent = do
  cb <- Apecs.get ent
  case cb of
    Attack -> playerAttack ent
    Invalid -> Broker.notify "You can't go that way."
    PickUp -> playerPickUp ent
    DoNothing -> pure ()

movePlayer ::
  ( Has (State GameState) sig m,
    Has Random sig m,
    MonadIO m
  ) =>
  V2 Int ->
  WorldT m ()
movePlayer dx = do
  debug <- use @GameState #debugMode
  offset <- (if debug then pure else Position.offsetRandomly) dx

  Apecs.modify player (offset +)
  Apecs.set player Flag.Dirty

recalculateInfo ::
  ( MonadIO m,
    Has (State GameState) sig m
  ) =>
  WorldT m Game.Info
recalculateInfo = do
  (hp :: HP, gold, xp, pos) <- Apecs.get player
  -- use the info as the accumulator itself
  -- when there's a Selection present, emit something other than mempty
  let zero =
        mempty
          & Info.hitpoints .~ hp
          & Info.gold .~ gold
          & Info.xp .~ xp
          & Info.position .~ pos

  let go inf e = inf & Info.enemyAt (e ^. typed) % _Enemy .~ e

  Apecs.cfold go zero

player :: Entity
player = Apecs.global

playerPosition :: MonadIO m => WorldT m Position
playerPosition = Apecs.get player

occupant :: MonadIO m => Position -> WorldT m (Maybe Apecs.Entity)
occupant p = fmap snd . getAlt <$> Apecs.cfoldMap go
  where
    go :: (Position, Apecs.Entity) -> Alt Maybe (Position, Apecs.Entity)
    go x = x <$ guard (fst x == p)

occupied :: MonadIO m => Position -> WorldT m Bool
occupied = fmap isJust . occupant
