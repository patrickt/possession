{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Game.State (State (State)) where

import Apecs qualified
import Optics
import GHC.Generics (Generic)

data State = State
  { statePlayer :: Apecs.Entity
  } deriving Generic

makeFieldLabels ''State
