{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module UI.InGame
  ( InGame (InGame),
    initial,
  )
where

import Brick qualified
import Brick.Widgets.Border qualified as Brick
import Data.Maybe
import GHC.Generics (Generic)
import Game.Action qualified as Action
import Game.Canvas (Canvas)
import Game.Canvas qualified as Canvas
import Graphics.Vty as Vty
import Linear (V2 (..))
import Optics
import UI.Attributes qualified as Attributes
import UI.Input qualified as Input
import UI.MainMenu qualified as MainMenu
import UI.Render
import UI.Responder
import UI.Sidebar (Sidebar)
import UI.Widgets.Modeline (Modeline)
import UI.Widgets.Modeline qualified as Modeline

data InGame = InGame
  { canvas :: Canvas,
    sidebar :: Sidebar,
    modeline :: Modeline
  }
  deriving stock (Generic)

makeFieldLabelsWith noPrefixFieldLabels ''InGame

initial :: InGame
initial =
  InGame Canvas.empty mempty Modeline.initial

instance Responder InGame where
  translate (Vty.EvKey k mods) _ = fromMaybe Input.None (Input.fromVty k mods)
  translate _ _ = Input.None

  onSend inp _s =
    let move (x, y) = Action.Move (V2 x y)
     in case inp of
          Input.Left -> Broadcast (move (-1, 0))
          Input.Right -> Broadcast (move (1, 0))
          Input.Down -> Broadcast (move (0, 1))
          Input.Up -> Broadcast (move (0, -1))
          Input.Quit -> Terminate
          Input.Menu -> Push (SomeResponder MainMenu.initial)
          Input.Look -> Push (error "unimplemented!")
          _ -> Nil

instance Renderable InGame where
  render _ = Brick.txt "bug: canvas needs renderMany"
  renderMany s =
    [ Attributes.withStandard . Brick.border . Brick.vBox $
        [ Brick.hBox
            [ Brick.hLimit 25 . Brick.border . render . view #sidebar $ s,
              Brick.border . Brick.padBottom Brick.Max . render . view #canvas $ s
            ],
          Brick.hBorder,
          Brick.vLimit 3 . render . view #modeline $ s
        ]
    ]
