{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

-- A 2-tuple of integers for position on the world grid.
module Data.Position
  ( Position (..),
    V2 (..),
    pos,
    components,
    make,
    offset,
    randomIn,
    brickLocation,
  )
where

import Control.Effect.Random
import Brick qualified
import Data.Generics.Product
import Data.Ix
import Linear.V2
import Optics

newtype Position = Position (V2 Int)
  deriving stock (Eq, Ord, Show)
  deriving newtype (Ix, Num)

_Position :: Iso' Position (V2 Int)
_Position = coerced

instance Field1 Position Position Int Int where
  _1 = _Position % lensVL _x

instance Field2 Position Position Int Int where
  _2 = _Position % lensVL _y

brickLocation :: Position -> Brick.Location
brickLocation (Position (V2 a b)) = Brick.Location (a + 1, b + 1)

pos :: forall a. HasType Position a => Lens' a Position
pos = typed

components :: Iso' Position (Int, Int)
components = iso (\(Position (V2 a b)) -> (a, b)) (\(a, b) -> Position (V2 a b))

make :: Int -> Int -> Position
make x y = Position (V2 x y)

offset :: V2 Int -> Position -> Position
offset v (Position p) = Position (v + p)

randomIn :: Has Random sig m => Int -> Int -> m Position
randomIn x y = make <$> uniformR (x, y) <*> uniformR (x, y)
