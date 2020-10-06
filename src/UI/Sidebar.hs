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
import Data.Generics.Product
import Data.Hitpoints
import GHC.Generics (Generic)
import Game.Info
import Graphics.Vty qualified as Vty
import Graphics.Vty.Attributes qualified as Attr
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
render sb =
  Brick.vBox
    [ Markup.markup renderedHP
    ]
  where
    renderedHP = case sb ^. field @"info" % hitpoints of
      Nothing -> boldhp <> "- / -"
      Just (HP curr max') -> boldhp <> (showt curr @? "green") <> " / " <> (showt max' @? "green")
    boldhp = "HP: " @? "bold"
    allAttrs =
      Brick.applyAttrMappings
