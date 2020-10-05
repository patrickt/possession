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
import Data.Position

newtype Glyph = Glyph Char deriving newtype (Show)

data Color = Black | Grey | White deriving (Show)

data Player = Player

data Wall = Wall

makeWorldAndComponents "World" [''Position, ''Glyph, ''Color, ''Player, ''Wall]

deriving newtype instance
  Eff.Algebra sig m =>
  Eff.Algebra (Eff.Reader World Eff.:+: sig) (SystemT World m)
