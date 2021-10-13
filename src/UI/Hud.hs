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
  )
where

import Brick qualified
import Brick.Widgets.Border qualified as Brick
import Data.Message qualified as Message
import Data.Position (Position)
import Data.Position qualified as Position
import GHC.Records
import Game.Canvas (Canvas)
import Game.Canvas qualified as Canvas
import Game.Info (Info)
import Linear (V2 (..))
import Optics
import UI.Attributes qualified as Attributes
import UI.Input qualified as Input
import UI.Render
import UI.Resource qualified as Resource
import UI.Responder
import UI.Widgets.Sidebar (Sidebar)
import UI.Widgets.Modeline (Modeline)
import UI.Widgets.Modeline qualified as Modeline

data Hud p = Hud
  { position :: Position,
    parent :: p
  }

makeFieldLabelsWith noPrefixFieldLabels ''Hud

instance
  forall p.
  ( HasField "canvas" p Canvas,
    HasField "sidebar" p Sidebar,
    HasField "modeline" p Modeline
  ) =>
  Renderable (Hud p)
  where
  render = error "not implemented"
  -- render stack s =
  --   [ Attributes.withStandard . Brick.border . Brick.vBox $
  --       [ Brick.hBox
  --           [ Brick.hLimit 25
  --               . Brick.border
  --               . render @Sidebar
  --               . getField @"sidebar"
  --               . parent
  --               $ s,
  --             Brick.showCursor Resource.Look (s ^. #position % to (Position.brickLocation . Canvas.clamp))
  --               . Brick.border
  --               . Brick.padBottom Brick.Max
  --               . Brick.reportExtent Resource.Canvas
  --               . render @Canvas
  --               . getField @"canvas"
  --               . parent
  --               $ s
  --           ],
  --         Brick.hBorder,
  --         render @Modeline
  --           . insertReadout (s ^. #position) (s & parent & getField @"sidebar" & view #info)
  --           . getField @"modeline"
  --           . parent
  --           $ s
  --       ] <> stack
  --   ]

insertReadout :: Position -> Info -> Modeline -> Modeline
insertReadout p i m = case i ^. #summary % at p of
  Just n -> m & Modeline.update (Message.youSee n)
  _ -> m

instance Responder (Hud p) where
  onSend inp inf s = case inp of
    Input.Left -> Update (s & #position %~ Position.offset (V2 (-1) 0))
    Input.Right -> Update (s & #position %~ Position.offset (V2 1 0))
    Input.Up -> Update (s & #position %~ Position.offset (V2 0 (-1)))
    Input.Down -> Update (s & #position %~ Position.offset (V2 0 1))
    Input.Quit -> Pop
    _ -> Nil
