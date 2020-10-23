{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

module Data.Experience where

import Data.Either.Validation
import Data.List (genericLength)
import Data.Maybe
import Data.Monoid
import Data.Monoid.Generic
import Data.Semigroup (Max)
import Dhall qualified
import Dhall.Exts ()
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Optics.IxFold

-- toNextLevel is dumb, it can be computed statically
-- and then everything here gets simpler
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

level :: XP -> Natural
level (XP (Sum curr) _) =
  let within (x, y) = curr >= x && curr < y
      indices = [10, 25, 45, 70, 100, 140] :: [Natural]
      paired = zip (0 : indices) indices
      orMax = fromMaybe (genericLength indices + 1) . getFirst
      go idx minmax acc
        | within minmax = acc <> pure (fromIntegral idx + 1)
        | otherwise = acc
   in orMax $ ifoldr go (First Nothing) paired
