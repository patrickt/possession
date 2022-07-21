{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module UI.Render
  ( Renderable (..),
    laidOut,
    RTree (..),
    (<+>),
    (<=>),
  runDraw)
where

import Brick qualified
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.Maybe (fromMaybe)
import Data.Message
import Data.Message qualified as Message
import Data.Semigroup
import Optics
import Data.Color qualified as Color
import TextShow
import UI.Resource
import UI.Markup as Markup
import qualified Brick.Widgets.Border as Brick
import Control.Applicative
import Control.Monad
import Game.Info (Info)

data RTree
  = Leaf Widget
  | HSplit RTree !RTree
  | VSplit RTree !RTree
  | Modal (Maybe RTree) !RTree

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
          Info -> Markup.text
          Warning -> Markup.fg Color.Yellow
          Danger -> Markup.fg Color.Red
          Debug -> Markup.fg Color.Green
        toAppend = case m ^. #times of
          1 -> ""
          n -> " (" <> showt (getSum n) <> "x)"
     in pure $ markup ((Message.contents m & foreground) <> Markup.text toAppend)
