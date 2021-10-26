{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
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

newtype Hud = Hud
  { hudPosition :: Position
  }

makeFieldLabels ''Hud

instance Renderable Hud
  where
  render s stack = do
    let loc = s ^. #position % to (Position.brickLocation . Canvas.clamp)
    stack & _head %~ Brick.showCursor Resource.Look loc



insertReadout :: Position -> Info -> Modeline -> Modeline
insertReadout p i m = case i ^. #summary % at p of
  Just n -> m & Modeline.update (Message.youSee n)
  _ -> m

instance Responder Hud where
  onSend inp _inf s = case inp of
    Input.Left -> Update (s & #position %~ Position.offset (V2 (-1) 0))
    Input.Right -> Update (s & #position %~ Position.offset (V2 1 0))
    Input.Up -> Update (s & #position %~ Position.offset (V2 0 (-1)))
    Input.Down -> Update (s & #position %~ Position.offset (V2 0 1))
    Input.Quit -> Pop
    _ -> Nil
