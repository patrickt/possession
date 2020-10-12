{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Aeson.Exts (module Data.Aeson) where

import Data.Aeson
import Data.Monoid

deriving newtype instance ToJSON a => ToJSON (Sum a)
deriving newtype instance FromJSON a => FromJSON (Sum a)
