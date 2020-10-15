{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.Callbacks where

import Dhall qualified
import Data.Text (Text)
import Data.Either.Validation

data Collision
  = Invalid
  | Attack
  | PickUp

data Callbacks = Callbacks
  { onCollision :: Collision
  }

hostile :: Callbacks
hostile = Callbacks (Attack)

instance Dhall.FromDhall Collision where
  autoWith n = Dhall.strictText { Dhall.extract = extract }
    where
      extract e = case Dhall.extract (Dhall.autoWith @Text n) e of
        Failure v -> Failure v
        Success t -> case t of
          "invalid" -> pure Invalid
          "attack"  -> pure Attack
          "pickup"  -> pure PickUp
          x -> Dhall.extractError ("Unrecognized collision behavior: " <> x)
