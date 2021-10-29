{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- Behavior values determine what entities do when other entities interact with them.
module Game.Behavior
  ( Collision (..),
  )
where

import Data.Either.Validation
import Data.Text (Text)
import Dhall qualified
import GHC.Generics (Generic)
import Data.Store (Store)
import Apecs (Component (..), Map)

data Collision
  = Invalid
  | Attack
  | PickUp
  deriving stock (Show, Generic)
  deriving anyclass (Store)

instance Component Collision where type Storage Collision = Map Collision

instance Dhall.FromDhall Collision where
  autoWith n = Dhall.strictText {Dhall.extract = extract}
    where
      extract e = case Dhall.extract (Dhall.autoWith @Text n) e of
        Failure v -> Failure v
        Success t -> case t of
          "invalid" -> pure Invalid
          "attack" -> pure Attack
          "pickup" -> pure PickUp
          x -> Dhall.extractError ("Unrecognized collision behavior: " <> x)
