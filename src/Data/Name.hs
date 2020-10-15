{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
-- | A type for the names of items and monsters.
module Data.Name ( Name (..) )where

import Data.Text (Text)
import Data.String (IsString)
import Dhall (FromDhall)

newtype Name = Name { text :: Text }
  deriving stock (Eq, Ord)
  deriving newtype (Show, IsString, FromDhall)
