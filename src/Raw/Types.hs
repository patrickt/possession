{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -ddump-splices #-}
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

[] <$ mapM addDependentFile
  [ "cfg/Collision.dhall"
  , "cfg/Color.dhall"
  , "cfg/Strategy.dhall"
  , "cfg/Enemy.dhall"
  ]

Dhall.TH.makeHaskellTypes
  [ MultipleConstructors "Collision" "./cfg/Collision.dhall",
    MultipleConstructors "Color" "./cfg/Color.dhall",
    MultipleConstructors "Strategy" "./cfg/Strategy.dhall",
    SingleConstructor "Enemy" "Enemy" "./cfg/Enemy.dhall"
  ]

deriving instance Show Color

deriving instance Store Collision
deriving instance Store Color
deriving instance Store Strategy

instance Component Color where type Storage Color = Map Color
instance Component Collision where type Storage Collision = Map Collision
instance Component Strategy where type Storage Strategy = Map Strategy

deriving instance Store Enemy
makeFieldLabelsNoPrefix ''Enemy
