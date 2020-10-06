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
import Data.Glyph
import Control.Carrier.Reader qualified as Eff
import Data.Hitpoints
import Data.Position
import Data.Color

data Player = Player

data Wall = Wall

makeWorldAndComponents
  "World"
  [ ''Color,
    ''Glyph,
    ''HP,
    ''Player,
    ''Position,
    ''Wall
  ]

deriving newtype instance
  Eff.Algebra sig m =>
  Eff.Algebra (Eff.Reader World Eff.:+: sig) (SystemT World m)
