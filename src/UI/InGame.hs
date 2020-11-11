{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.InGame where

import Brick qualified
import Brick.Widgets.Border qualified as Brick
import Data.Generics.Product
import GHC.Generics (Generic)
import Game.Canvas (Canvas)
import Game.Canvas qualified as Canvas
import Optics
import UI.Attributes qualified as Attributes
import UI.Render
import UI.Responder
import UI.Sidebar (Sidebar)
import UI.Widgets.Modeline (Modeline)
import UI.Widgets.Modeline qualified as Modeline
import UI.Input qualified as Input

data InGame = InGame
  { _canvas :: Canvas,
    _sidebar :: Sidebar,
    _modeline :: Modeline
  }
  deriving stock (Generic)

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
  translate _ _ = Input.None
  onSend _ _ = Nil

instance Renderable InGame where
  render _ = Brick.txt "bug: canvas needs renderMany"
  renderMany s =
    [ Attributes.withStandard . Brick.border . Brick.vBox $
        [ Brick.hBox
            [ Brick.hLimit 25 . Brick.border . render . view sidebar $ s,
              Brick.border . Brick.padBottom Brick.Max . render . view canvas $ s
            ],
          Brick.hBorder,
          Brick.vLimit 3 . render . view modeline $ s
        ]
    ]
