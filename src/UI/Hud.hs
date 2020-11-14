{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UI.Hud
  ( Hud,
    initial,
  )
where

import qualified Brick
import Brick.Widgets.Border qualified as Brick
import Data.Position (Position)
import Data.Position qualified as Position
import Game.Canvas (Canvas)
import GHC.Records
import Optics
import UI.Attributes qualified as Attributes
import UI.Render
import UI.Resource qualified as Resource
import UI.Widgets.Modeline (Modeline)
import UI.Sidebar (Sidebar)
import UI.Responder
import UI.Widgets.Modeline qualified as Modeline
import Data.Maybe
import UI.Input qualified as Input

import Graphics.Vty qualified as Vty
import Linear (V2 (..))

data Hud p = Hud
  { position :: Position
  , parent :: p
  }

makeFieldLabelsWith noPrefixFieldLabels ''Hud

getCanvas :: HasField "canvas" p Canvas => Hud p -> Canvas
getCanvas = getField @"canvas" . parent

getSidebar :: HasField "sidebar" p Sidebar => Hud p -> Sidebar
getSidebar = getField @"sidebar" . parent

initial :: a -> Hud a
initial = Hud (Position.make 5 5)

instance forall p . (HasField "canvas" p Canvas, HasField "sidebar" p Sidebar) => Renderable (Hud p) where
  render _ = Brick.txt "error: needs renderMany"
  renderMany s =
    [ Brick.showCursor Resource.Look (s ^. #position % to Position.brickLocation) . Attributes.withStandard . Brick.border . Brick.vBox $
        [ Brick.hBox
            [ Brick.hLimit 25 . Brick.border . render . getSidebar $ s,
              Brick.border . Brick.padBottom Brick.Max . Brick.reportExtent Resource.Canvas . render . getCanvas $ s
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
