{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A type for the names of items and monsters.
module Data.Name (Name (..)) where

import Data.String (IsString)
import Data.Text (Text)
import Dhall (FromDhall)

newtype Name = Name {text :: Text}
  deriving stock (Eq, Ord)
  deriving newtype (Show, IsString, FromDhall)
