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
import Data.Monoid
import UI.Sidebar (Sidebar)
import UI.Widgets.Modeline (Modeline)
import UI.Widgets.Modeline qualified as Modeline
import UI.Hud qualified as Hud

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
  translate (Vty.EvKey k mods) _ = case k of
    Vty.KChar 'l' -> Input.Look
    _ -> fromMaybe Input.None (Input.fromVty k mods)
  translate _ _ = Input.None

  onSend inp s =
    let move (x, y) = Broadcast (Action.Move (V2 x y))
        left = move (-1, 0)
        right = move (1, 0)
        down = move (0, 1)
        up = move (0, -1)
     in case inp of
          Input.Left  -> left
          Input.Right -> right
          Input.Down  -> down
          Input.Up    -> up
          Input.Quit -> Terminate
          Input.Menu -> Push (SomeResponder MainMenu.inGame)
          Input.Look -> Push (SomeResponder (Hud.Hud (s ^. #sidebar % #info % #position % to getLast % non 0) s))
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
          render . view #modeline $ s
        ]
    ]
