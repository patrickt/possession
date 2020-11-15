{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A type for the names of items and monsters.
module Data.Name (Name (..), definiteArticle) where

import Data.Char qualified as Char
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as Text
import Dhall (FromDhall)
import Data.Store (Store)

newtype Name = Name {text :: Text}
  deriving stock (Eq, Ord)
  deriving newtype (Show, IsString, FromDhall, Store)

definiteArticle :: Name -> Text
definiteArticle (Name n)
  | Char.toLower (Text.head n) `elem` ("aeiou" :: String) = "an"
  | otherwise = "a"
