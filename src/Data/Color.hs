{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Color where

import GHC.Generics (Generic)
import Data.Either.Validation
import Dhall qualified
import Data.Text (Text)

data Color = Black | Grey | White | Yellow
  deriving (Show, Generic)

instance Dhall.FromDhall Color where
  autoWith n = Dhall.strictText { Dhall.extract = extract }
    where
      extract e = case Dhall.extract (Dhall.autoWith @Text n) e of
        Failure v -> Failure v
        Success t -> case t of
          "black" -> pure Black
          "grey"  -> pure Grey
          "gray"  -> pure Grey
          "white" -> pure White
          "yellow" -> pure Yellow
          x -> Dhall.extractError ("Unrecognized color name: " <> x)
