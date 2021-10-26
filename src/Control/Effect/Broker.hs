{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- An effect that provides bidirectional communication between the
-- game and its Brick UI. Brick itself reads @Action 'UI@ values from
-- a BChan and then invokes the event loop itself; we have to do a
-- little more work, but it works on the same principle.
module Control.Effect.Broker
  ( Broker,
    pushAction,
    popAction,
    sendCommand,
    runBroker,
    notify,
    Brokerage (Brokerage),
    enqueueGameAction,
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
import Optics.TH

data Brokerage = Brokerage
  { _brokerageBrickQueue :: BChan (Action 'UI),
    _brokerageGameQueue :: TBQueue (Action 'Game)
  }

enqueueGameAction :: Brokerage -> Action 'Game -> IO ()
enqueueGameAction (Brokerage _ q) act = atomically (writeTBQueue q act)

makeFieldLabels ''Brokerage

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

runBroker :: Brokerage -> BrokerC m a -> m a
runBroker b = runReader b . runBrokerC

instance Has (Lift IO) sig m => Algebra (Broker :+: sig) (BrokerC m) where
  alg hdl sig ctx = do
    Brokerage to curr <- BrokerC ask
    case sig of
      L (Push a) -> ctx <$ (sendM . atomically . writeTBQueue curr $ a)
      L (Cmd a) -> ctx <$ (sendM . writeBChan to $ a)
      L Pop -> (<$ ctx) <$> (sendM . atomically . readTBQueue $ curr)
      R other -> BrokerC (alg (runBrokerC . hdl) (R other) ctx)
