{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game.Canvas where

import Data.Array (Array, array, (//), (!))
import Game.World (Glyph (..), Position(..), Color (..))
import Data.Maybe
import Linear

data Sprite = Sprite
  { glyph :: Glyph
  , color :: Color
  } deriving Show

blankSprite :: Sprite
blankSprite = Sprite (Glyph '.') White

size :: Int
size = 4

newtype Canvas = Canvas { unCanvas :: Array Position (Maybe Sprite) }
  deriving newtype Show

empty :: Canvas
empty = Canvas $ array (0 :: Position, Position (V2 size size)) do
  x <- [0..size]
  y <- [0..size]
  pure (Position (V2 x y), Just blankSprite)

update :: Canvas -> [(Position, Sprite)] -> Canvas
update (Canvas arr) assocs = Canvas (arr // new)
  where new = fmap (fmap Just) assocs

at :: Canvas -> Position -> Sprite
at (Canvas canv) pos = fromMaybe blankSprite it
  where it = canv ! pos
