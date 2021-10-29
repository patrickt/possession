{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Color (Color (..)) where

import Apecs (Component (..), Map)
import Data.Either.Validation
import Data.Store (Store)
import Data.Text (Text)
import Dhall qualified
import GHC.Generics (Generic)

-- | A UI-independent notion of color.
data Color = Black | Grey | White | Yellow | Brown
  deriving stock (Show, Generic)
  deriving anyclass (Store)

instance Component Color where type Storage Color = Map Color

-- | Serialized as descriptive strings.
instance Dhall.FromDhall Color where
  autoWith n = Dhall.strictText {Dhall.extract = extract}
    where
      extract e = case Dhall.extract (Dhall.autoWith @Text n) e of
        Failure v -> Failure v
        Success t -> case t of
          "black" -> pure Black
          "grey" -> pure Grey
          "gray" -> pure Grey
          "white" -> pure White
          "yellow" -> pure Yellow
          "brown" -> pure Yellow
          x -> Dhall.extractError ("Unrecognized color name: " <> x)
