{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Raws
  ( Raws
  , loadFromDhall
  , getRaw
  )
where

import Optics hiding (use)
import Control.Effect.Optics
import Raw.Types
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Data.Store.Exts (Store)
import qualified Dhall
import Control.Effect.State
import qualified Raw.Id as Raw
import qualified Data.Vector as Vector

data Raws = Raws
  { rawsEnemies :: Vector Enemy
  , rawsItems   :: Vector Item
  }
  deriving stock Generic
  deriving anyclass Store

makeFieldLabels ''Raws

getRaw :: Has (State Raws) sig m => Lens' Raws (Vector a) -> Raw.Id -> m a
getRaw target (Raw.Id ident) = do
  needed <- use target
  pure (Vector.unsafeIndex needed ident)

loadFromDhall :: IO Raws
loadFromDhall = Raws
  <$> Dhall.inputFile Dhall.auto "cfg/all_enemies.dhall"
  <*> Dhall.inputFile Dhall.auto "cfg/all_items.dhall"
