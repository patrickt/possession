{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -ddump-splices #-}

-- | The current state of the game, /independent of the values in the ECS/.
-- This is mainly suitable for holding configuration values, cached values
-- like the player's 'Entity', et cetera. It is generally read-only (and
-- should perhaps be called something else.
module Game.State (State (State), player, debugMode) where

import Apecs qualified
import Data.Generics.Product.Typed
import GHC.Generics (Generic)
import Optics

data State = State
  { statePlayer :: Apecs.Entity,
    stateDebugMode :: Bool
  }
  deriving (Generic)

player :: Lens' State Apecs.Entity
player = typed

debugMode :: Lens' State Bool
debugMode = typed
