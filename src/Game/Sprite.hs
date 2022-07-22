module Game.Sprite
  ( Sprite (Sprite)
  , blankSprite
  ) where

import Raw.Types (Color (..))
import Data.Glyph

-- Glyph is wrong here, we should parameterize this or something.

data Sprite = Sprite Glyph (Maybe Color) (Maybe Color)
  deriving Show

blankSprite :: Sprite
blankSprite = Sprite (Glyph '.') Nothing Nothing
