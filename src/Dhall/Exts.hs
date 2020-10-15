{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dhall.Exts where

import Data.Monoid
import Data.Semigroup (Max (..))
import Dhall

deriving newtype instance ToDhall a => ToDhall (Sum a)

deriving newtype instance ToDhall a => ToDhall (Max a)

deriving newtype instance FromDhall a => FromDhall (Sum a)

deriving newtype instance FromDhall a => FromDhall (Max a)
