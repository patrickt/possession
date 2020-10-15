{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Effect.Broker
  ( Broker,
    pushAction,
    popAction,
    sendCommand,
    runBroker,
  notify)
where

import Brick.BChan
import Control.Algebra
import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Concurrent
import Control.Monad.IO.Class
import Data.Kind (Type)
import Game.Action (Action, Dest (..))
import qualified Game.Action as Game
import Data.Message (Message)

data Brokerage = Brokerage
  { _toBrick :: BChan (Action 'UI),
    _currentAction :: MVar (Action 'Game)
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

runBroker :: BChan (Action 'UI) -> MVar (Action 'Game) -> BrokerC m a -> m a
runBroker to curr = runReader (Brokerage to curr) . runBrokerC

instance Has (Lift IO) sig m => Algebra (Broker :+: sig) (BrokerC m) where
  alg hdl sig ctx = do
    Brokerage to curr <- BrokerC ask
    case sig of
      L (Push a) -> ctx <$ (sendM . putMVar curr $ a)
      L (Cmd a) -> ctx <$ (sendM . writeBChan to $ a)
      L Pop -> (<$ ctx) <$> (sendM . takeMVar $ curr)
      R other -> BrokerC (alg (runBrokerC . hdl) (R other) ctx)
