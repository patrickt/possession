{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module UI.Hud
  ( Hud (Hud),
    initial,
  )
where

import Brick qualified
import Brick.Widgets.Border qualified as Brick
import Data.Maybe
import Data.Position (Position)
import Data.Position qualified as Position
import GHC.Records
import Game.Canvas (Canvas)
import Graphics.Vty qualified as Vty
import Linear (V2 (..))
import Optics
import Game.Canvas qualified as Canvas
import UI.Attributes qualified as Attributes
import UI.Input qualified as Input
import UI.Render
import UI.Resource qualified as Resource
import UI.Responder
import UI.Sidebar (Sidebar)

data Hud p = Hud
  { position :: Position,
    parent :: p
  }

makeFieldLabelsWith noPrefixFieldLabels ''Hud

initial :: a -> Hud a
initial = Hud (Position.make 5 5)

instance forall p. (HasField "canvas" p Canvas, HasField "sidebar" p Sidebar) => Renderable (Hud p) where
  render _ = Brick.txt "error: needs renderMany"
  renderMany s =
    [ Attributes.withStandard . Brick.border . Brick.vBox $
        [ Brick.hBox
            [ Brick.hLimit 25
                . Brick.border
                . render @Sidebar
                . getField @"sidebar"
                . parent
                $ s,
              Brick.showCursor Resource.Look (s ^. #position % to (Position.brickLocation . (+ 1) . Canvas.clamp))
                . Brick.border
                . Brick.padBottom Brick.Max
                . Brick.reportExtent Resource.Canvas
                . render @Canvas
                . getField @"canvas"
                . parent
                $ s
            ],
          Brick.hBorder
        ]
    ]

instance Responder (Hud p) where
  translate (Vty.EvKey k mods) _ = fromMaybe Input.None (Input.fromVty k mods)
  translate _ _ = Input.None

  onSend inp s = case inp of
    Input.Left -> Update (s & #position %~ Position.offset (V2 (-1) 0))
    Input.Right -> Update (s & #position %~ Position.offset (V2 1 0))
    Input.Up -> Update (s & #position %~ Position.offset (V2 0 (-1)))
    Input.Down -> Update (s & #position %~ Position.offset (V2 0 1))
    Input.Quit -> Pop
    _ -> Nil
