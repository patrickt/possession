{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module UI.Render where

import Game.Canvas qualified as Canvas
import Game.Canvas qualified as Game (Canvas)
import Game.World qualified as World
import Graphics.Vty qualified as Vty
import Graphics.Vty.Attributes qualified as Attr
import Linear (V2 (..))
import UI.Resource
import Brick qualified

drawSprite :: Canvas.Sprite -> Vty.Image
drawSprite (Canvas.Sprite (World.Glyph chr) color) = Vty.char attr chr
  where
    attr = Attr.currentAttr {Attr.attrForeColor = Attr.SetTo (colorToVty color)}

colorToVty :: World.Color -> Vty.Color
colorToVty = \case
  World.Black -> Attr.black
  World.Grey -> Attr.rgbColor 221 221 (221 :: Int)
  World.White -> Attr.white

scanline :: Int -> Game.Canvas -> Vty.Image
scanline idx canv = do
  let scanlines = do
        x <- [0 .. Canvas.size]
        pure (Canvas.at canv (World.Position (V2 x idx)))
  let squares = fmap drawSprite scanlines
  Vty.horizCat squares

render :: Game.Canvas -> Brick.Widget Resource
render canv =
  let allLines = [scanline x canv | x <- [0 .. Canvas.size]]
  in Brick.viewport UI.Resource.Canvas Brick.Both
     . Brick.raw
     $ Vty.vertCat allLines
