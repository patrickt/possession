{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PatternSynonyms #-}

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
    bounds,
    clamp,
    at,
    size,
    Sprite (Sprite),
  )
where

import Data.Position (Position, pattern (:-))
import Data.Position qualified as Position
import Game.Sprite
import Data.IntMap.Strict (IntMap)
import Data.IntMap qualified as IntMap

newtype Canvas = Canvas (IntMap Sprite)
  deriving newtype (Show)

empty :: Canvas
empty = Canvas $ IntMap.fromList do
  x <- [0 .. size]
  y <- [0 .. size]
  pure (locate x y, blankSprite)

update :: Canvas -> Position -> Sprite -> Canvas
update (Canvas arr) pos spr = Canvas (IntMap.insert (locatePos pos) spr arr)

-- This calls 'error' when given an invalid position.
at :: Canvas -> Position -> Sprite
at (Canvas canv) pos = canv IntMap.! locatePos pos

locate :: Int -> Int -> Int
locate x y = (x * size) + y

locatePos :: Position -> Int
locatePos (x :- y) = (x * size) + y

size :: Int
size = 60

bounds :: (Position, Position)
bounds = (0 :: Position, fromIntegral size)

clamp :: Position -> Position
clamp (x :- y) = go x :- go y
  where
    go n
      | n <= 0 = 0
      | n > size = size
      | otherwise = n

borders :: [Position]
borders = up <> down <> left <> right
  where
    up = Position.V2 <$> horizontal <*> pure 0
    down = Position.V2 <$> horizontal <*> pure size
    left = Position.V2 0 <$> vertical
    right = Position.V2 size <$> vertical
    horizontal = [0 .. size]
    vertical = [1 .. size -1]
