{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module Data.Experience where

import Data.Monoid
import Data.Monoid.Generic
import Data.Semigroup (Max)
import Dhall (FromDhall)
import Dhall.Exts ()
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

data XP = XP {_current :: Sum Natural, _toNextLevel :: Max Natural}
  deriving stock (Generic)
  deriving anyclass (FromDhall)
  deriving (Semigroup) via GenericSemigroup XP

instance Monoid XP where mempty = XP 0 0
