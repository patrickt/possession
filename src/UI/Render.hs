{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Render (Renderable (..), renderOne, colorToVty, withForeground) where

import Brick qualified
import Brick.Markup
import Data.Color qualified as Color
import Data.Message
import Data.Semigroup
import Data.Text.Markup qualified as Markup
import Graphics.Vty qualified as Vty
import Graphics.Vty.Attributes qualified as Attr
import Optics
import TextShow
import UI.Resource

class Renderable a where
  render :: a -> [Brick.Widget Resource] -> [Brick.Widget Resource]

renderOne :: Renderable a => a -> Brick.Widget Resource
renderOne a = head (render a [])

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
