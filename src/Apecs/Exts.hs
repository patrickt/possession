{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Apecs.Exts
  ( module Apecs,
    cfoldMap,
    append,
    remove,
    removing,
    tupled,
  )
where

import Apecs
import Apecs.Core
import qualified Apecs.THTuples
import Data.Generics.Sum
import Data.Maybe (fromJust)
import Optics

tupled :: forall ctor a b. AsConstructor ctor a a b b => a -> b
tupled = fromJust . preview (_Ctor @ctor)

cfoldMap :: forall w m c a. (Apecs.Members w m c, Apecs.Get w m c, Monoid a) => (c -> a) -> Apecs.SystemT w m a
cfoldMap f = Apecs.cfold (\a b -> a <> f b) mempty

append :: forall w m cx. (Get w m cx, Set w m cx, Semigroup cx) => Entity -> cx -> SystemT w m ()
append e v = Apecs.modify e (<> v)

remove :: forall c w m. Destroy w m c => Entity -> SystemT w m ()
remove e = destroy e (Proxy @c)

removing :: forall c w m unused. Destroy w m c => Iso' unused c -> Entity -> SystemT w m ()
removing _ = remove @c @w @m

Apecs.THTuples.makeInstances [9 .. 14]
