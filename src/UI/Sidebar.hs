{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
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
import TextShow (showt)
import Game.Info
import Graphics.Vty qualified as Vty
import Graphics.Vty.Attributes qualified as Attr
import Optics
import UI.Resource
import Data.Hitpoints
import GHC.Generics (Generic)

data Sidebar = Sidebar
  { info :: Info
  } deriving Generic

initial :: Sidebar
initial = Sidebar mempty

render :: Sidebar -> Brick.Widget Resource
render sb =
  Brick.updateAttrMap allAttrs $
    Brick.vBox
      [ Markup.markup renderedHP
      ]
  where
    renderedHP = case sb ^. field @"info" % hitpoints % coercedTo @(Maybe HP) of
      Nothing -> boldhp <> "- / -"
      Just (HP curr max') -> boldhp <> (showt curr @? "green") <> " / " <> (showt max' @? "green")
    boldhp = "HP: " @? "bold"
    allAttrs =
      Brick.applyAttrMappings
      [ ("green", Brick.fg Vty.green)
      , ("bold", Attr.withStyle Attr.defAttr Attr.bold)
      ]
