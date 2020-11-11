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
module Game.State (State (State)) where

import Apecs qualified
import GHC.Generics (Generic)
import Optics

data State = State
  { player :: Apecs.Entity,
    debugMode :: Bool
  }
  deriving (Generic)

makeFieldLabelsWith noPrefixFieldLabels ''State
