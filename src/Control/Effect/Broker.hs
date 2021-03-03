{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- An effect that provides bidirectional communication between the
-- game and its Brick UI. Brick itself reads @Action 'UI@ values from
-- a BChan and then invokes the event loop itself; we have to do a
-- little more work, but it works on the same principle.
--
-- TODO: should the game channel be a chan rather than an mvar?
module Control.Effect.Broker
  ( Broker,
    BrickQueue,
    GameQueue,
    pushAction,
    popAction,
    sendCommand,
    runBroker,
    notify,
  )
where

import Brick.BChan
import Control.Algebra
import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue
import Control.Monad.IO.Class
import Data.Kind (Type)
import Data.Message (Message)
import Game.Action (Action, Dest (..))
import Game.Action qualified as Game

type BrickQueue = BChan (Action 'UI)

type GameQueue = TBQueue (Action 'Game)

data Brokerage = Brokerage
  { _toBrick :: BrickQueue,
    _toGame :: GameQueue
  }

data Broker (m :: Type -> Type) k where
  Push :: Action 'Game -> Broker m ()
  Pop :: Broker m (Action 'Game)
  Cmd :: Action 'UI -> Broker m ()

-- | Blocks if the queue is full
pushAction :: Has Broker sig m => Action 'Game -> m ()
pushAction = send . Push

popAction :: Has Broker sig m => m (Action 'Game)
popAction = send Pop

sendCommand :: Has Broker sig m => Action 'UI -> m ()
sendCommand = send . Cmd

notify :: Has Broker sig m => Message -> m ()
notify = sendCommand . Game.Notify

newtype BrokerC m a = BrokerC {runBrokerC :: ReaderC Brokerage m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

runBroker :: BrickQueue -> GameQueue -> BrokerC m a -> m a
runBroker to curr = runReader (Brokerage to curr) . runBrokerC

instance Has (Lift IO) sig m => Algebra (Broker :+: sig) (BrokerC m) where
  alg hdl sig ctx = do
    Brokerage to curr <- BrokerC ask
    case sig of
      L (Push a) -> ctx <$ (sendM . atomically . writeTBQueue curr $ a)
      L (Cmd a) -> ctx <$ (sendM . writeBChan to $ a)
      L Pop -> (<$ ctx) <$> (sendM . atomically . readTBQueue $ curr)
      R other -> BrokerC (alg (runBrokerC . hdl) (R other) ctx)
