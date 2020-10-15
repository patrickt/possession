{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
module Data.Name ( Name (..) )where

import Data.Text (Text)
import Data.String (IsString)

newtype Name = Name { text :: Text }
  deriving stock (Eq, Ord)
  deriving newtype (Show, IsString)
