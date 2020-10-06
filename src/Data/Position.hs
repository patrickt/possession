{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Position
  ( Position (..),
    V2 (..),
    pos,
    make,
    offset,
  )
where

import Data.Ix
import Linear.V2
import Data.Generics.Product
import Optics

newtype Position = Position (V2 Int)
  deriving stock (Eq, Ord, Show)
  deriving newtype (Ix, Num)

pos :: forall a . HasType Position a => Lens' a Position
pos = typed

make :: Int -> Int -> Position
make x y = Position (V2 x y)

offset :: V2 Int -> Position -> Position
offset v (Position p) = Position (v + p)
