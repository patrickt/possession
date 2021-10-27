{-# LANGUAGE AllowAmbiguousTypes #-}
module Apecs.Exts
  ( module Apecs
  , cfoldMap
  , append
  , remove
  ) where

import Apecs

cfoldMap :: forall w m c a. (Apecs.Members w m c, Apecs.Get w m c, Monoid a) => (c -> a) -> Apecs.SystemT w m a
cfoldMap f = Apecs.cfold (\a b -> a <> f b) mempty

append :: forall w m cx. (Get w m cx, Set w m cx, Semigroup cx) => Entity -> cx -> SystemT w m ()
append e v = Apecs.modify e (<> v)

remove :: forall c w m. Destroy w m c => Entity -> SystemT w m ()
remove e = destroy e (Proxy @c)
