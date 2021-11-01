module Game.Sprite
  ( Sprite (Sprite)
  , blankSprite
  ) where

import Raw.Types (Color (..))
import Data.Glyph

data Sprite = Sprite Glyph Color
  deriving Show

blankSprite :: Sprite
blankSprite = Sprite (Glyph '.') White
