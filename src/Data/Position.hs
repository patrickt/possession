{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BlockArguments #-}

-- A 2-tuple of integers for position on the world grid.
module Data.Position
  ( V2 (..),
    type Position,
    HasPosition(..),
    pattern (:-),
    randomIn,
    brickLocation,
    adjacentClamped,
    offset,
    offsetRandomly,
    stepTowards,
    dist,
  )
where

import Apecs (Component (..), Map, Cache)
import Brick qualified
import Control.Effect.Random
import Data.Generics.Product hiding (HasPosition (..))
import Linear
import Optics
import TextShow

type Position = V2 Int

instance TextShow Position where
  showb (a :- b) = "(" <> showb a <> "," <> showb b <> ")"

instance Apecs.Component Position where type Storage Position = Cache 3600 (Map Position)

pattern (:-) :: Int -> Int -> Position
pattern (:-) a b = V2 a b

{-# COMPLETE (:-) #-}

brickLocation :: Iso' Position Brick.Location
brickLocation = iso fore aft where
  fore (a :- b) = Brick.Location (a, b)
  aft (Brick.Location (a, b)) = a :- b

class HasPosition a where
  position :: Lens' a Position

  default position :: HasType Position a => Lens' a Position
  position = typed

instance HasPosition Position where position = castOptic simple

adjacentClamped :: Int -> Position -> [Position]
adjacentClamped maxim o@(a :- b) = filter scanner do
  x <- [-1, 0, 1]
  y <- [-1, 0, 1]
  pure ((a + x) :- (b + y))
  where
    scanner v = v `isBoundedBy` (maxim :- maxim) && v /= o


isBoundedBy :: Position -> Position -> Bool
isBoundedBy (a :- b) (maxX :- maxY) = a >= 0 && b >= 0 && a < maxX && b < maxY

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

stepTowards :: Position -> Position -> V2 Int
stepTowards a b =
  fmap (round @_ @Int)
    . normalize
    . fmap (fromIntegral @_ @Double)
    $ (a - b)

dist :: Position -> Position -> Double
dist a b = distance (fromIntegral <$> a) (fromIntegral <$> b)
