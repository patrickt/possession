{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The current state of the game, /independent of the values in the ECS/.
-- This is mainly suitable for holding configuration values, cached values
-- like the player's 'Entity', et cetera. It is generally read-only (and
-- should perhaps be called something else. (Game.Env?)
module Game.State (State, initial) where

import Apecs (Entity)
import Data.Map.Strict (Map)
import Data.Position (Position)
import GHC.Generics (Generic)
import Optics
import Game.Info
import Data.Generics.Product (typed)

data State = State
  { stateDebugMode :: Bool,
    stateAtlas :: Map Position Entity,
    stateInfo :: Info
  }
  deriving stock (Generic)

instance HasInfo State where info = typed

initial :: State
initial = State True mempty mempty

makeFieldLabels ''State
