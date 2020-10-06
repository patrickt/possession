{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -ddump-splices #-}

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
