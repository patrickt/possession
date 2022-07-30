module Game.Entity.Inventory
  ( Inventory (..)
  )
where

import Apecs qualified
import Data.Store.Exts (Store)
import qualified Raw.Id as Raw

newtype Inventory = Inventory [Raw.Id]
  deriving newtype Store

instance Apecs.Component Inventory where
  type Storage Inventory = Apecs.Map Inventory
