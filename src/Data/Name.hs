{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A type for the names of items and monsters.
module Data.Name (Name (..), definiteArticle, HasName (..)) where

import Data.Char qualified as Char
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as Text
import Dhall (FromDhall)
import Data.Store (Store)
import Apecs (Component (..), Map)
import Optics
import Data.Generics.Product.Typed

newtype Name = Name {text :: Text}
  deriving stock (Eq, Ord)
  deriving newtype (Show, IsString, FromDhall, Store)

instance Apecs.Component Name where type Storage Name = Map Name

class HasName a where
  name :: Lens' a Name

  default name :: HasType Name a => Lens' a Name
  name = typed

definiteArticle :: Name -> Text
definiteArticle (Name n)
  | Char.toLower (Text.head n) `elem` ("aeiou" :: String) = "an"
  | otherwise = "a"
