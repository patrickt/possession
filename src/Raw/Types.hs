{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}
module Raw.Types
  ( module Raw.Types
  , module Raw.Id
  ) where

import Apecs.Exts
import Dhall.TH
import Optics.TH (makeFieldLabelsNoPrefix)
import Raw.Id
import Data.Store.Exts (Store)
import Language.Haskell.TH.Syntax (addDependentFile)
--
[] <$ mapM addDependentFile
  [ "cfg/Collision.dhall"
  , "cfg/Color.dhall"
  , "cfg/Strategy.dhall"
  , "cfg/Enemy.dhall"
  , "cfg/ItemProperty.dhall"
  , "cfg/Item.dhall"
  ]

Dhall.TH.makeHaskellTypes
  [ MultipleConstructors "Collision" "./cfg/Collision.dhall",
    MultipleConstructors "Color" "./cfg/Color.dhall",
    MultipleConstructors "Strategy" "./cfg/Strategy.dhall",
    MultipleConstructors "ItemProperty" "./cfg/ItemProperty.dhall",
    SingleConstructor "Enemy" "Enemy" "./cfg/Enemy.dhall",
    SingleConstructor "Item" "Item" "./cfg/Item.dhall"
  ]

deriving instance Show Color
deriving instance Eq Strategy
deriving instance Eq ItemProperty

deriving instance Store Collision
deriving instance Store Color
deriving instance Store Strategy
deriving instance Store ItemProperty

instance Component Color where type Storage Color = Map Color
instance Component Collision where type Storage Collision = Map Collision
instance Component Strategy where type Storage Strategy = Map Strategy
instance Component ItemProperty where type Storage ItemProperty = Map ItemProperty

deriving instance Store Enemy
deriving instance Store Item

makeFieldLabelsNoPrefix ''Enemy
makeFieldLabelsNoPrefix ''Item
