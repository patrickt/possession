module Game.Sprite
  ( Sprite (Sprite)
  , blankSprite
  ) where

import Data.Color
import Data.Glyph

data Sprite = Sprite
  { glyph :: !Glyph,
    color :: !Color
  }
  deriving (Show)

blankSprite :: Sprite
blankSprite = Sprite (Glyph '.') White
