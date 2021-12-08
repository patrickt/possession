{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Game.Flag
  ( module Game.Flag,
  )
where

import Apecs (Component (..), Map)
import Apecs qualified
import Control.Monad qualified
import Data.Proxy
import Data.Store.Exts (Store)
import GHC.Generics (Generic)

class Enum a => Flag a

data Digger = Digger
  deriving (Eq, Show, Enum, Flag)

data Persist = Persist
  deriving stock (Eq, Show, Enum, Generic)
  deriving anyclass (Store)

instance Component Persist where type Storage Persist = Map Persist

data Dirty = Dirty
  deriving stock (Eq, Show, Enum, Generic)
  deriving anyclass (Flag, Store)

instance Component Dirty where type Storage Dirty = Map Dirty

data Impassable = Impassable
  deriving stock (Eq, Show, Enum, Generic)
  deriving anyclass (Flag, Store)

instance Component Impassable where type Storage Impassable = Map Impassable

get :: forall f w m. (Apecs.Get w m f, Flag f) => Apecs.Entity -> Apecs.SystemT w m Bool
get e = Apecs.exists e (Proxy @f)

set :: forall f w m. (Apecs.Set w m f, Flag f) => Apecs.Entity -> Apecs.SystemT w m ()
set e = Apecs.set e (toEnum @f 0)

when :: forall f w m. (Apecs.Get w m f, Flag f) => Apecs.Entity -> Apecs.SystemT w m () -> Apecs.SystemT w m ()
when e fn = do
  on <- get @f e
  Control.Monad.when on fn
