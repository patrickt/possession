{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- A 2-tuple of integers for position on the world grid.
module Data.Position
  ( V2 (..),
    type Position,
    pattern (:-),
    position,
    randomIn,
    brickLocation,
    offset,
    offsetRandomly,
  )
where

import Brick qualified
import Control.Effect.Random
import Data.Generics.Product hiding (position)
import Linear.V2
import Optics
import Apecs (Component (..), Map)

type Position = V2 Int

instance Apecs.Component Position where type Storage Position = Map Position

pattern (:-) :: Int -> Int -> Position
pattern (:-) a b = V2 a b

{-# COMPLETE (:-) #-}

brickLocation :: Position -> Brick.Location
brickLocation (a :- b) = Brick.Location (a + 1, b + 1)

position :: forall a. HasType Position a => Lens' a Position
position = typed

randomIn :: Has Random sig m => Int -> Int -> m Position
randomIn x y = V2 <$> uniformR (x, y) <*> uniformR (x, y)

offset :: Int -> Int -> Position -> Position
offset x y (V2 a b) = V2 (x + a) (y + b)

offsetRandomly :: Has Random sig m => V2 Int -> m (V2 Int)
offsetRandomly (V2 x y) = V2 <$> go x <*> go y
  where
    go v = do
      fuzz <- uniformR (0, 2)
      degree <- uniformR (1, 3)
      pure ((v + fuzz) * degree)
