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
import Data.Functor.Identity

data RTree
  = Leaf Widget
  | HSplit RTree RTree
  | VSplit RTree RTree
  | Modal (Maybe RTree) RTree

makeBaseFunctor ''RTree

class Renderable a where
  {-# MINIMAL layout | draw #-}
  layout :: Monad m => a -> m RTree
  layout = fmap Leaf . draw

  draw :: Monad m => a -> m Widget
  draw = layout >=> draw

runDraw :: Renderable a => a -> Widget
runDraw = runIdentity . draw

(<+>), (<=>) :: Applicative m => m Widget -> m Widget -> m Widget
(<+>) = liftA2 (Brick.<+>)
(<=>) = liftA2 (Brick.<=>)

instance Renderable () where draw = const (pure Brick.emptyWidget)

laidOut :: (Monad m, Renderable a) => Getter a (m RTree)
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
