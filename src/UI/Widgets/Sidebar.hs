{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module UI.Widgets.Sidebar
  ( Sidebar (Sidebar),
    initial,
  )
where

import Brick qualified
import Brick.Markup qualified as Markup
import Brick.Util (fg)
import Data.Experience (XP (..))
import Data.Experience qualified as Experience
import Data.Hitpoints
import Data.Monoid
import Data.Semigroup (Max (..))
import Data.Text.Markup ((@@))
import Data.Text.Markup qualified as Markup
import GHC.Generics (Generic)
import Game.Info
import Graphics.Vty qualified as Vty
import Optics
import TextShow (showt)
import UI.Render
import Control.Effect.Reader

newtype Sidebar = Sidebar ()
  deriving (Generic)

makeFieldLabels ''Sidebar

initial :: Sidebar
initial = Sidebar mempty

instance Renderable Sidebar where
  draw _ = do
    i <- ask @Info
    let
      boxed =
        Brick.vBox $
          fmap
            Markup.markup
            [ renderedHP,
              renderedGold,
              renderedLevel,
              renderedXP
            ]

      renderedHP = case i ^. #hitpoints % coerced of
        Nothing -> boldhp <> "- / -"
        Just (HP curr max') -> boldhp <> (showt curr @@ fg Vty.green) <> " / " <> (showt max' @@ fg Vty.green)
      boldhp = "HP: " @@ bold

      bold = mempty `Vty.withStyle` Vty.bold

      renderedGold = ("GP: " @@ bold) <> (showt (i ^. #gold % to getSum) @@ fg Vty.yellow)

      renderedXP = case i ^. #xp of
        XP (Sum curr) (Max next) -> ("XP: " @@ bold) <> showm curr <> " (next: " <> showm next <> ")"

      renderedLevel = ("Level: " @@ bold) <> showm (i ^. #xp & Experience.level)

      showm = Markup.fromText . showt
    pure boxed
