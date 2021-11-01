{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Control.Carrier.Trace.Brokered
  ( runTrace,
    TraceC (..),
    module Control.Effect.Trace,
  )
where

import Control.Algebra
import Control.Effect.Broker
import Control.Effect.Trace
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Message as Message

-- | Run a 'Trace' effect, ignoring all traces.
--
-- @
-- 'runTrace' ('trace' s) = 'pure' ()
-- @
-- @
-- 'runTrace' ('pure' a) = 'pure' a
-- @
--
-- @since 1.0.0.0
runTrace :: TraceC m a -> m a
runTrace (TraceC m) = m
{-# INLINE runTrace #-}

-- | @since 1.0.0.0
newtype TraceC m a = TraceC { runTraceC :: m a }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance (Has Broker sig m, Algebra sig m) => Algebra (Trace :+: sig) (TraceC m) where
  alg hdl sig ctx = case sig of
    L (Trace s) -> ctx <$ TraceC (notify (Message.debug s))
    R other -> TraceC (alg (runTrace . hdl) other ctx)
  {-# INLINE alg #-}
