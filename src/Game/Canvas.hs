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
  }

blankSprite :: Sprite
blankSprite = Sprite (Glyph ' ') Black

newtype Canvas = Canvas { unCanvas :: Array Position (Maybe Sprite) }

empty :: Canvas
empty = Canvas $ array (0 :: Position, 80 :: Position) do
  x <- [0..80]
  y <- [0..80]
  pure (Position (V2 x y), Nothing)

update :: Canvas -> [(Position, Sprite)] -> Canvas
update (Canvas arr) assocs = Canvas (arr // new)
  where new = fmap (fmap Just) assocs

at :: Canvas -> Position -> Sprite
at (Canvas canv) pos = fromMaybe blankSprite it
  where it = canv ! pos
