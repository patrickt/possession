{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Raws
  ( Raws
  , loadFromDhall
  )
where

import Optics
import Raw.Types
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Data.Store.Exts (Store)
import qualified Dhall

newtype Raws = Raws
  { rawsEnemies :: Vector Enemy
  }
  deriving stock Generic
  deriving anyclass Store

makeFieldLabels ''Raws

loadFromDhall :: IO Raws
loadFromDhall = Raws
  <$> Dhall.inputFile Dhall.auto "cfg/all_enemies.dhall"
