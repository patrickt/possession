{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- Behavior values determine what entities do when other entities interact with them.
module Game.Behavior
  ( Collision (..),
  )
where

import Data.Either.Validation
import Data.Text (Text)
import Dhall qualified

data Collision
  = Invalid
  | Attack
  | PickUp

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
