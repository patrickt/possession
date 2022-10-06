{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}

-- An effect that provides bidirectional communication between the
-- game and its Brick UI. Brick itself reads @Action 'UI@ values from
-- a BChan and then invokes the event loop itself; we have to do a
-- little more work, but it works on the same principle.
module Control.Effect.Broker
  ( Broker,
    runBroker,
    pushAction,
    popAction,
    pushToUI,
    notify,
    -- * Types
    Brokerage(..),
    newBrokerage,
    -- * Internal
    enqueueGameAction,
  )
where

import Brick.BChan
import Control.Algebra
import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Concurrent.Chan.Unagi.Bounded
import Control.Monad.IO.Class
import Data.Coerce
import Data.Kind (Type)
import Data.Message (Message)
import Game.Action (Action, Dest (..))
import Game.Action qualified as Game
import Optics

-- | Mediates channels between UI, Brick backend, and ECS. Created with 'newBrokerage'.
data Brokerage = Brokerage
  { brickQueue :: BChan (Action 'UI),
    gameWrite :: InChan (Action 'Game),
    gameRead :: OutChan (Action 'Game)
  }

makeFieldLabelsNoPrefix ''Brokerage

newBrokerage :: IO Brokerage
newBrokerage = do
  brickQueue <- newBChan 1
  (gameWrite, gameRead) <- newChan 100
  pure Brokerage{..}

-- | A backdoor for when you need to send a message out-of-band.
enqueueGameAction :: Brokerage -> Action 'Game -> IO ()
enqueueGameAction b = writeChan (b ^. #gameWrite)

-- | Effect that lets you send 'Action' values down the appropriate pipes.
-- All effects block if their associated queue is full.
data Broker (m :: Type -> Type) k where
  Push :: Action 'Game -> Broker m ()
  Pop :: Broker m (Action 'Game) -- not used for now
  Cmd :: Action 'UI -> Broker m ()

-- Enqueue a 'GameAction' within the ECS.
pushAction :: Has Broker sig m => Action 'Game -> m ()
pushAction a = send (Push a)

-- Read an enqueued 'GameAction'.
popAction :: Has Broker sig m => m (Action 'Game)
popAction = send Pop

-- Send a 'UIAction' to the
pushToUI :: Has Broker sig m => Action 'UI -> m ()
pushToUI a = send (Cmd a)

-- Helper to send a 'Game.Notify' message.
notify :: Has Broker sig m => Message -> m ()
notify a = pushToUI (Game.Notify a)

newtype BrokerC m a = BrokerC (ReaderC Brokerage m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO)

runBroker :: Brokerage -> BrokerC m a -> m a
runBroker b (BrokerC r) = runReader b r
{-# INLINE runBroker #-}

instance Has (Lift IO) sig m => Algebra (Broker :+: sig) (BrokerC m) where
  alg hdl sig ctx = do
    Brokerage brick writer reader <- BrokerC ask
    case sig of
      L (Push a) -> ctx <$ (sendM . writeChan writer $ a)
      L (Cmd a) -> ctx <$ (sendM . writeBChan brick $ a)
      L Pop -> (<$ ctx) <$> (sendM . readChan $ reader)
      R other -> BrokerC (alg (coerce . hdl) (R other) ctx)
