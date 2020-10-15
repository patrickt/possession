{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Game.World (module Game.World) where

import Apecs
import Control.Algebra qualified as Eff
import Control.Carrier.Reader qualified as Eff
import Data.Color
import Data.Glyph
import Data.Hitpoints
import Data.Amount
import Data.Name (Name)
import Data.Position
import Game.Callbacks

data Wall = Wall

makeWorldAndComponents
  "World"
  [ ''Amount,
    ''Color,
    ''Collision,
    ''Glyph,
    ''HP,
    ''Name,
    ''Position
  ]

deriving newtype instance
  Eff.Algebra sig m =>
  Eff.Algebra (Eff.Reader World Eff.:+: sig) (SystemT World m)
