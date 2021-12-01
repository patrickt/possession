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
    (<+>),
    (<=>),
  runDraw)
where

import Brick qualified
import Brick.Markup
import Brick.Util (fg)
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
import qualified Brick.Widgets.Border as Brick
import Control.Applicative
import Control.Monad
import Game.Info (Info)

data RTree
  = Leaf Widget
  | HSplit RTree RTree
  | VSplit RTree RTree
  | Modal (Maybe RTree) RTree

makeBaseFunctor ''RTree

type RenderM = ((->) Info)

class Renderable a where
  {-# MINIMAL layout | draw #-}
  layout :: a -> RenderM RTree
  layout = fmap Leaf . draw

  draw :: a -> RenderM Widget
  draw = layout >=> draw

runDraw :: Renderable a => Info -> a -> Widget
runDraw = flip draw

(<+>), (<=>) :: Applicative m => m Widget -> m Widget -> m Widget
(<+>) = liftA2 (Brick.<+>)
(<=>) = liftA2 (Brick.<=>)

instance Renderable () where draw = const (pure Brick.emptyWidget)

laidOut :: Renderable a => Getter a (RenderM RTree)
laidOut = to layout

instance Renderable RTree where
  layout = pure
  draw = cataA \case
    LeafF f -> pure f
    HSplitF a b -> a <+> pure Brick.vBorder <+> b
    VSplitF a b -> a <=> pure Brick.hBorder <=> b
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
     in pure $ markup ((Message.contents m @@ foreground) <> Markup.fromText toAppend)

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
