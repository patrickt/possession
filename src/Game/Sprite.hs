module Game.Sprite
  ( Sprite (Sprite)
  , blankSprite
  ) where

import Raw.Types (Color (..))
import Data.Glyph

-- Glyph is wrong here, we should parameterize this or something.

data Sprite = Sprite Glyph Color Color
  deriving Show

blankSprite :: Sprite
blankSprite = Sprite (Glyph '.') White Black
