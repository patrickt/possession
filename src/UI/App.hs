{-# LANGUAGE ImportQualifiedPost #-}

module UI.App (app) where

import Brick qualified
import Brick.AttrMap qualified as Brick.AttrMap
import Brick.Forms qualified as Form
import Graphics.Vty qualified as Vty
import UI.MainMenu qualified as MainMenu
import UI.Resource qualified as UI (Resource)
import UI.State qualified as State
import UI.State qualified as UI (State)

draw :: UI.State -> [Brick.Widget UI.Resource]
draw s = case State.mode s of
  State.InMenu -> [Form.renderForm (MainMenu.form (State.mainMenu s))]

app :: Brick.App UI.State e UI.Resource
app =
  Brick.App
    { Brick.appDraw = draw,
      Brick.appChooseCursor = Brick.neverShowCursor,
      Brick.appHandleEvent = Brick.resizeOrQuit,
      Brick.appAttrMap = const (Brick.AttrMap.attrMap Vty.defAttr []),
      Brick.appStartEvent = pure
    }
