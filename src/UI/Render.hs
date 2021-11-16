{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module UI.Render
  ( Renderable (..),
    laidOut,
    RTree (..),
    withForeground,
    colorToVty,
  )
where

import Brick qualified
import Brick.Markup
import Brick.Util (fg)
import Brick.Widgets.Core ((<+>), (<=>))
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.Maybe (fromMaybe)
import Data.Message
import Data.Message qualified as Message
import Data.Semigroup
import Data.Text.Markup ((@@))
import Data.Text.Markup qualified as Markup
import Graphics.Vty qualified as Vty
import Graphics.Vty.Attributes qualified as Attr
import Optics
import Raw.Types qualified as Color (Color (..))
import TextShow
import UI.Resource

data RTree
  = Leaf Widget
  | HSplit RTree RTree
  | VSplit RTree RTree
  | Modal (Maybe RTree) RTree

makeBaseFunctor ''RTree

class Renderable a where
  {-# MINIMAL layout | draw #-}
  layout :: a -> RTree
  layout = Leaf . draw

  draw :: a -> Widget
  draw = draw . layout

instance Renderable () where draw = const Brick.emptyWidget

laidOut :: Renderable a => Getter a RTree
laidOut = to layout

instance Renderable RTree where
  layout = id
  draw = cata \case
    LeafF f -> f
    HSplitF a b -> a <+> b
    VSplitF a b -> a <=> b
    ModalF a b -> fromMaybe b a

instance Renderable Message where
  draw m =
    let foreground = case m ^. #urgency % coerced of
          Info -> mempty
          Warning -> fg Vty.yellow
          Danger -> fg Vty.red
          Debug -> fg Vty.green
        toAppend = case m ^. #times of
          1 -> ""
          n -> " (" <> showt (getSum n) <> "x)"
     in markup ((Message.contents m @@ foreground) <> Markup.fromText toAppend)

withForeground :: Color.Color -> Brick.Widget a -> Brick.Widget a
withForeground color = Brick.modifyDefAttr attr
  where
    attr a = a {Attr.attrForeColor = Attr.SetTo (colorToVty color)}

colorToVty :: Color.Color -> Vty.Color
colorToVty = \case
  Color.Black -> Vty.black
  Color.Grey -> Vty.rgbColor @Int 221 221 221
  Color.White -> Vty.white
  Color.Red -> Vty.red
  Color.Yellow -> Vty.brightYellow
  Color.Brown -> Vty.rgbColor @Int 0x78 0x58 0x32
