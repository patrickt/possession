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
import Data.Semigroup
import Linear

deriving newtype instance Store a => Store (Max a)
deriving anyclass instance Store a => Store (V2 a)
