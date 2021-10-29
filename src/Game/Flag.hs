{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Game.Flag
    ( module Game.Flag
    ) where

import Apecs qualified
import Control.Monad qualified
import Data.Proxy
import Data.Store.Exts (Store)
import GHC.Generics (Generic)

class Enum a => Flag a where

data Digger = Digger
  deriving (Eq, Show, Enum, Flag)

data Persist = Persist
  deriving stock (Eq, Show, Enum, Generic)
  deriving anyclass Store

data Dirty = Dirty
  deriving stock (Eq, Show, Enum, Generic)
  deriving anyclass (Flag, Store)

get :: forall f w m . (Apecs.Get w m f, Flag f) => Apecs.Entity -> Apecs.SystemT w m Bool
get e = Apecs.exists e (Proxy @f)

set :: forall f w m . (Apecs.Set w m f, Flag f) => Apecs.Entity -> Apecs.SystemT w m ()
set e = Apecs.set e (toEnum @f 0)

when :: forall f w m . (Apecs.Get w m f,Flag f) => Apecs.Entity -> Apecs.SystemT w m () -> Apecs.SystemT w m ()
when e fn = do
  on <- get @f e
  Control.Monad.when on fn
