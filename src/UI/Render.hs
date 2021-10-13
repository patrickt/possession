{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Render (Renderable (..), colorToVty, withForeground) where

import Brick qualified
import Brick.Markup
import Data.Color qualified as Color
import Data.Glyph
import Data.Message
import Data.Position qualified as Position
import Data.Semigroup
import Data.Text.Markup qualified as Markup
import Game.Canvas qualified as Canvas
import Game.Canvas qualified as Game (Canvas)
import Graphics.Vty qualified as Vty
import Graphics.Vty.Attributes qualified as Attr
import Optics
import TextShow
import UI.Resource

class Renderable a where
  render :: a -> [Brick.Widget Resource] -> [Brick.Widget Resource]


instance Renderable Message where
  render m stack =
    let attr = case m ^. #urgency % coerced of
          Info -> ""
          Warning -> "yellow"
          Danger -> "red"
        toAppend = case m ^. #times of
          1 -> ""
          n -> " (" <> showt (getSum n) <> "x)"
        final = markup (((m ^. #contents % coerced) @? attr) <> Markup.fromText toAppend)
     in final : stack

instance Renderable Game.Canvas where
  render canv stack =
    let allLines = scanline canv <$> [0 .. Canvas.size]
        final =
          Brick.viewport UI.Resource.Canvas Brick.Both
            . Brick.raw
            $ Vty.vertCat allLines
     in final : stack



drawSprite :: Canvas.Sprite -> Vty.Image
drawSprite (Canvas.Sprite (Glyph chr) color) = Vty.char attr chr
  where
    attr = Attr.currentAttr {Attr.attrForeColor = Attr.SetTo (colorToVty color)}

scanline :: Game.Canvas -> Int -> Vty.Image
scanline canv idx = Vty.horizCat do
  x <- [0 .. Canvas.size]
  pure . drawSprite . Canvas.at canv $ Position.make x idx

withForeground :: Color.Color -> Brick.Widget a -> Brick.Widget a
withForeground color = Brick.modifyDefAttr attr
  where
    attr a = a {Attr.attrForeColor = Attr.SetTo (colorToVty color)}

colorToVty :: Color.Color -> Vty.Color
colorToVty = \case
  Color.Black -> Vty.black
  Color.Grey -> Vty.rgbColor 221 221 (221 :: Int)
  Color.White -> Vty.white
  Color.Yellow -> Vty.brightYellow
  Color.Brown -> Vty.rgbColor @Int 0x78 0x58 0x32
