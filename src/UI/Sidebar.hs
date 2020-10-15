{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module UI.Sidebar
  ( Sidebar (Sidebar),
    initial,
    render,
  )
where

import Brick qualified
import Brick.Markup ((@?))
import Brick.Markup qualified as Markup
import Data.Experience (XP (..))
import Data.Generics.Product
import Data.Hitpoints
import Data.Monoid
import Data.Semigroup (Max (..))
import Data.Text.Markup qualified as Markup
import GHC.Generics (Generic)
import Game.Info
import Optics
import TextShow (showt)
import UI.Resource

data Sidebar = Sidebar
  { info :: Info
  }
  deriving (Generic)

initial :: Sidebar
initial = Sidebar mempty

render :: Sidebar -> Brick.Widget Resource
render (Sidebar i) =
  Brick.vBox
    [ Markup.markup renderedHP,
      Markup.markup renderedGold,
      Markup.markup renderedXP
    ]
  where
    renderedHP = case i ^. hitpoints of
      Nothing -> boldhp <> "- / -"
      Just (HP curr max') -> boldhp <> (showt curr @? "green") <> " / " <> (showt max' @? "green")
    boldhp = "HP: " @? "bold"

    renderedGold = ("GP: " @? "bold") <> (showt (i ^. gold) @? "yellow")

    renderedXP = case i ^. xp of
      XP (Sum curr) (Max next) -> ("XP: " @? "bold") <> showm curr <> " (next: " <> showm next <> ")"

    showm = Markup.fromText . showt
