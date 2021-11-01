module Raw.Id
  ( Id (..),
  )
where

import Apecs (Component (..), Map)
import Data.Store.Exts (Store)
import GHC.Generics (Generic)

newtype Id = Id Int
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (Store)

instance Component Id where type Storage Id = Map Id
