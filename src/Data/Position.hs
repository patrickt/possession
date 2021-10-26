{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- A 2-tuple of integers for position on the world grid.
module Data.Position
  ( V2 (..),
    type Position,
    pattern (:-),
    pos,
    randomIn,
    brickLocation,
    offset,
  )
where

import Brick qualified
import Control.Effect.Random
import Data.Generics.Product
import Linear.V2
import Optics

type Position = V2 Int

pattern (:-) :: Int -> Int -> Position
pattern (:-) a b = V2 a b
{-# COMPLETE (:-) #-}

brickLocation :: Position -> Brick.Location
brickLocation (a :- b) = Brick.Location (a + 1, b + 1)

pos :: forall a. HasType Position a => Lens' a Position
pos = typed

randomIn :: Has Random sig m => Int -> Int -> m Position
randomIn x y = V2 <$> uniformR (x, y) <*> uniformR (x, y)

offset :: Int -> Int -> Position -> Position
offset x y (V2 a b) = V2 (x + a) (y + b)
