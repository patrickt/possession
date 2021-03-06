{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- | A UI-agnostic representation of the current state of the
-- displayable world. Sent over the wire to the UI in a 'Command' to
-- trigger a redraw.
--
-- TODO: this does not use sharing very well.
module Game.Canvas
  ( Canvas (Canvas),
    empty,
    update,
    borders,
    clamp,
    at,
    size,
    Sprite (Sprite),
  )
where

import Data.Array (Array, array, (!), (//))
import Data.Position (Position (..), V2(..))
import Data.Position qualified as Position
import Game.Sprite

size :: Int
size = 60

bounds :: (Position, Position)
bounds = (0 :: Position, Position.make size size)

clamp :: Position -> Position
clamp (Position (V2 x y)) = Position (V2 (go x) (go y))
  where
    go n
      | n <= 0 = 0
      | n > size = size
      | otherwise = n

borders :: [Position]
borders = up <> down <> left <> right
  where
    up = Position.make <$> horizontal <*> pure 0
    down = Position.make <$> horizontal <*> pure size
    left = Position.make 0 <$> vertical
    right = Position.make size <$> vertical
    horizontal = [0 .. size]
    vertical = [1 .. size -1]

newtype Canvas = Canvas (Array Position Sprite)
  deriving newtype (Show)

empty :: Canvas
empty = Canvas $ array bounds do
  x <- [0 .. size]
  y <- [0 .. size]
  pure (Position.make x y, blankSprite)

update :: Canvas -> [(Position, Sprite)] -> Canvas
update (Canvas arr) assocs = Canvas (arr // assocs)

-- This calls 'error' when given an invalid position.
at :: Canvas -> Position -> Sprite
at (Canvas canv) = (canv !)
