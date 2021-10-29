{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | A natural number representing a quantity of something.
-- Used for things like value of gold pieces.
module Data.Amount (Amount (..)) where

import Dhall qualified
import Numeric.Natural
import System.Random.MWC.Exts (Variate (..))
import Data.Store (Store)
import TextShow (TextShow)
import Apecs (Component (..), Map)

newtype Amount = Amount {natural :: Natural}
  deriving stock (Eq, Show, Ord)
  deriving newtype (Enum, Real, Integral, Num, Dhall.FromDhall, Dhall.ToDhall, TextShow, Store)

instance Component Amount where type Storage Amount = Map Amount

-- 'coerce' isn't smart enough to let us derive this instance with the above
-- newtype declaration.
instance Variate Amount where
  uniform = fmap Amount . uniform
  uniformR (a, b) = fmap Amount . uniformR (natural a, natural b)
