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

import Prelude hiding ((&&))
import Brick qualified
import Data.Experience (XP (..))
import Data.Experience qualified as Experience
import Data.Color
import Data.Hitpoints
import Data.Monoid
import Data.Semigroup (Max (..))
import GHC.Generics (Generic)
import Game.Info
import Optics
import UI.Render
import UI.Markup
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
            markup
            [ renderedHP,
              renderedGold,
              renderedLevel,
              renderedXP
            ]

      renderedHP = case i ^. #hitpoints % coerced of
        Nothing -> boldhp <> "- / -"
        Just (HP curr max') -> boldhp <> curr && fg Green <> " / " <> max' && fg Green
      boldhp = "HP: " & bold

      renderedGold = ("GP: " & bold) <> (i ^. #gold % to getSum) && fg Yellow

      renderedXP = case i ^. #xp of
        XP (Sum curr) (Max next) -> ("XP: " & bold) <> shown curr <> " (next: " <> shown next <> ")"

      renderedLevel = ("Level: " & bold) <> (i ^. #xp & Experience.level & shown)
    pure boxed
