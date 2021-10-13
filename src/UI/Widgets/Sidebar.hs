{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module UI.Widgets.Sidebar
  ( Sidebar (Sidebar),
    render,
  )
where

import Brick qualified
import Brick.Markup ((@?))
import Brick.Markup qualified as Markup
import Data.Experience (XP (..))
import Data.Experience qualified as Experience
import Data.Hitpoints
import Data.Monoid
import Data.Monoid.Generic
import Data.Semigroup (Max (..))
import Data.Text.Markup qualified as Markup
import GHC.Generics (Generic)
import Game.Info
import Optics
import TextShow (showt)
import UI.Render

newtype Sidebar = Sidebar
  { sidebarInfo :: Info
  }
  deriving (Generic)
  deriving (Semigroup) via GenericSemigroup Sidebar
  deriving (Monoid) via GenericMonoid Sidebar

makeFieldLabels ''Sidebar

instance Renderable Sidebar where
  render (Sidebar i) stack = boxed : stack
    where
      boxed = Brick.vBox $
        fmap
          Markup.markup
          [ renderedHP,
            renderedGold,
            renderedLevel,
            renderedXP
          ]

      renderedHP = case i ^. #hitpoints % coerced of
        Nothing -> boldhp <> "- / -"
        Just (HP curr max') -> boldhp <> (showt curr @? "green") <> " / " <> (showt max' @? "green")
      boldhp = "HP: " @? "bold"

      renderedGold = ("GP: " @? "bold") <> (showt (i ^. #gold % to getSum) @? "yellow")

      renderedXP = case i ^. #xp of
        XP (Sum curr) (Max next) -> ("XP: " @? "bold") <> showm curr <> " (next: " <> showm next <> ")"

      renderedLevel = ("Level: " @? "bold") <> showm (i ^. #xp & Experience.level)

      showm = Markup.fromText . showt
