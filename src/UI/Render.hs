{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module UI.Render where

import Brick qualified
import Data.Position qualified as Position
import Data.Glyph
import Game.Canvas qualified as Canvas
import Game.Canvas qualified as Game (Canvas)
import Game.World qualified as World
import Data.Color (Color)
import Data.Color qualified as Color
import Graphics.Vty qualified as Vty
import Graphics.Vty.Attributes qualified as Attr
import UI.Resource

drawSprite :: Canvas.Sprite -> Vty.Image
drawSprite (Canvas.Sprite (Glyph chr) color) = Vty.char attr chr
  where
    attr = Attr.currentAttr {Attr.attrForeColor = Attr.SetTo (colorToVty color)}

colorToVty :: Color -> Vty.Color
colorToVty = \case
  Color.Black -> Attr.black
  Color.Grey -> Attr.rgbColor 221 221 (221 :: Int)
  Color.White -> Attr.white

scanline :: Int -> Game.Canvas -> Vty.Image
scanline idx canv = do
  let scanlines = do
        x <- [0 .. Canvas.size]
        pure (Canvas.at canv (Position.make x idx))
  let squares = fmap drawSprite scanlines
  Vty.horizCat squares

render :: Game.Canvas -> Brick.Widget Resource
render canv =
  let allLines = [scanline x canv | x <- [0 .. Canvas.size]]
   in Brick.viewport UI.Resource.Canvas Brick.Both
        . Brick.raw
        $ Vty.vertCat allLines
