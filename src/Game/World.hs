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
import Data.Position
import Game.Entity.Enemy qualified as Enemy
import Data.Text (Text)
import Game.Entity.Player qualified as Player
import Game.Callbacks (Callbacks)

data Wall = Wall

type CBE = Callbacks Enemy.Enemy

makeWorldAndComponents
  "World"
  [ ''Color,
    ''CBE,
    ''Enemy.Self,
    ''Glyph,
    ''HP,
    ''Player.Self,
    ''Text,
    ''Position,
    ''Wall
  ]

deriving newtype instance
  Eff.Algebra sig m =>
  Eff.Algebra (Eff.Reader World Eff.:+: sig) (SystemT World m)
