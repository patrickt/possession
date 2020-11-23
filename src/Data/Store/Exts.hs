{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Store.Exts
  ( module Data.Store
  ) where

import Data.Store
import Numeric.Natural
import Data.Functor.Contravariant
import Data.Semigroup
import Linear

instance Store Natural where
  size = contramap fromIntegral (size :: Size Integer)
  poke = poke . toInteger
  peek = fmap fromIntegral (peek :: Peek Integer)

deriving newtype instance Store a => Store (Max a)
deriving anyclass instance Store a => Store (V2 a)
