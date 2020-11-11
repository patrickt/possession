{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module UI.InGame where

import Brick qualified
import Data.Generics.Product
import Game.Canvas (Canvas)
import Game.Canvas qualified as Canvas
import Optics
import UI.Render
import UI.Responder
import UI.Sidebar (Sidebar)
import UI.Widgets.Modeline (Modeline)
import UI.Widgets.Modeline qualified as Modeline
import GHC.Generics (Generic)

data InGame = InGame
  { _canvas :: Canvas,
    _sidebar :: Sidebar,
    _modeline :: Modeline
  } deriving stock Generic

modeline :: Lens' InGame Modeline
modeline = typed

canvas :: Lens' InGame Canvas
canvas = typed

sidebar :: Lens' InGame Sidebar
sidebar = typed

initial :: InGame
initial =
  InGame
    { _canvas = Canvas.empty,
      _modeline = Modeline.initial,
      _sidebar = mempty
    }

instance Responder InGame where
  onSend _ _ = Nil

instance Renderable InGame where
  render _ = Brick.txt "ingame"
