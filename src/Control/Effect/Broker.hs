{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
module Control.Effect.Broker
  ( Broker
  , pushAction
  , popAction
  , sendCommand
  , runBroker
  )where

import Brick.BChan
import Control.Concurrent
import Game.Command (Command)
import Control.Carrier.Lift
import Control.Monad.IO.Class
import Game.Action
import Control.Carrier.Reader
import qualified Game.Command as Game
import qualified Game.Action as Game
import Data.Kind (Type)
import Control.Algebra

data Brokerage = Brokerage
  { _toBrick :: BChan Command
  , _currentAction :: MVar Action
  }

data Broker (m :: Type -> Type) k where
  Push :: Game.Action -> Broker m ()
  Pop :: Broker m Game.Action
  Cmd :: Game.Command -> Broker m ()

-- | Blocks if the queue is full
pushAction :: Has Broker sig m => Game.Action -> m ()
pushAction = send . Push

popAction :: Has Broker sig m => m Game.Action
popAction = send Pop

sendCommand :: Has Broker sig m => Game.Command -> m ()
sendCommand = send . Cmd


newtype BrokerC m a = BrokerC { runBrokerC :: ReaderC Brokerage m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)

runBroker :: BChan Command -> MVar Action -> BrokerC m a -> m a
runBroker to curr = runReader (Brokerage to curr) . runBrokerC

instance Has (Lift IO) sig m => Algebra (Broker :+: sig) (BrokerC m) where
  alg hdl sig ctx = do
    Brokerage to curr <- BrokerC ask
    case sig of
      L (Push a) -> ctx <$ (sendM . putMVar curr $ a)
      L (Cmd a) -> ctx <$ (sendM . writeBChan to $ a)
      L Pop -> (<$ ctx) <$> (sendM . takeMVar $ curr)
      R other   -> BrokerC (alg (runBrokerC . hdl) (R other) ctx)
