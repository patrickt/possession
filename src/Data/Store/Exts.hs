{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Instances not provided by Store.
module Data.Store.Exts
  ( module Data.Store
  ) where

import Data.Store
import Data.Semigroup
import Linear
import Apecs.Util (EntityCounter (..))

deriving newtype instance Store a => Store (Max a)
deriving newtype instance Store EntityCounter
deriving anyclass instance Store a => Store (V2 a)
