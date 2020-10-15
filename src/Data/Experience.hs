{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module Data.Experience where

import Data.Monoid
import Data.Monoid.Generic
import Data.Semigroup (Max)
import Dhall qualified
import Dhall.Exts ()
import GHC.Generics (Generic)
import Data.Either.Validation
import Numeric.Natural (Natural)

data XP = XP {_current :: Sum Natural, _toNextLevel :: Max Natural}
  deriving stock (Generic)
  deriving (Semigroup) via GenericSemigroup XP

instance Monoid XP where mempty = XP 0 0

instance Dhall.FromDhall XP where
  autoWith n = Dhall.natural {Dhall.extract = extract}
    where
      extract e = case Dhall.extract (Dhall.autoWith @Natural n) e of
        Failure v -> Failure v
        Success t -> pure (XP (pure t) 0)
