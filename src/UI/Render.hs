{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
module UI.Render (Renderable (..)) where

import Brick qualified
import Data.Color qualified as Color
import Data.Glyph
import Data.Position qualified as Position
import Game.Canvas qualified as Canvas
import Game.Canvas qualified as Game (Canvas)
import Graphics.Vty qualified as Vty
import Graphics.Vty.Attributes qualified as Attr
import UI.Resource

class Renderable a where
  render :: a -> Brick.Widget Resource

drawSprite :: Canvas.Sprite -> Vty.Image
drawSprite (Canvas.Sprite (Glyph chr) color) = Vty.char attr chr
  where
    attr = Attr.currentAttr {Attr.attrForeColor = Attr.SetTo (Color.toVty color)}

scanline :: Game.Canvas -> Int -> Vty.Image
scanline canv idx = Vty.horizCat do
  x <- [0 .. Canvas.size]
  pure . drawSprite . Canvas.at canv $ Position.make x idx

instance Renderable Game.Canvas where
  render canv =
    let allLines = scanline canv <$> [0 .. Canvas.size]
     in Brick.viewport UI.Resource.Canvas Brick.Both
          . Brick.raw
          $ Vty.vertCat allLines
