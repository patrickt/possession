{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module UI.Canvas
  ( Canvas (Canvas),
    initial,
    update
  )
where

import Brick qualified
import Data.Position
import Game.Canvas qualified
import Game.Canvas qualified as Game (Canvas)
import Graphics.Vty qualified as Vty
import Graphics.Vty.Attributes qualified as Attr
import Data.Glyph
import Optics
import UI.Render
import Game.Sprite
import UI.Resource qualified
import GHC.Generics (Generic)

data Canvas = Canvas
  { canvasData :: Game.Canvas,
    canvasShowsCursor :: Bool
  } deriving stock Generic

makeFieldLabels ''Canvas

initial :: Canvas
initial = Canvas { canvasData = Game.Canvas.empty, canvasShowsCursor = False }

instance Renderable Canvas where
  render canv stack =
    let allLines = scanline canv <$> [0 .. Game.Canvas.size]
        rendered =
          Brick.viewport UI.Resource.Canvas Brick.Both
            . Brick.raw
            $ Vty.vertCat allLines
     in rendered : stack

scanline :: Canvas -> Int -> Vty.Image
scanline (Canvas canv _) idx = Vty.horizCat do
  x <- [0 .. Game.Canvas.size]
  pure . drawSprite . Game.Canvas.at canv $ x :- idx

drawSprite :: Sprite -> Vty.Image
drawSprite (Sprite (Glyph chr) color) = Vty.char attr chr
  where
    attr = Attr.currentAttr {Attr.attrForeColor = Attr.SetTo (colorToVty color)}

update :: Game.Canvas -> Canvas -> Canvas
update = set #data
