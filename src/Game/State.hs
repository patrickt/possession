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

import GHC.Generics (Generic)
import Optics
import Game.Canvas qualified as Canvas

data State = State
  { stateDebugMode :: Bool,
    stateCanvas :: Canvas.Canvas
  }
  deriving (Generic)

initial :: State
initial = State True Canvas.empty

makeFieldLabels ''State
