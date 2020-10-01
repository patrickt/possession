{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Game.State where

import Apecs qualified
import GHC.Generics (Generic)

data State = State
  { player :: Apecs.Entity
  } deriving Generic
