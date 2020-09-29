{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module UI.App (app) where

import Brick qualified
import Brick.AttrMap qualified as Brick.AttrMap
import Brick.Forms qualified as Form
import Data.Generics.Product.Typed
import Data.Maybe
import Debug.Trace
import Game.Action qualified as Action
import Game.Action qualified as Game (Command)
import Graphics.Vty qualified as Vty
import Optics
import UI.Input qualified as Input
import UI.MainMenu qualified as MainMenu
import UI.Render qualified as Render
import UI.Resource qualified as UI (Resource)
import UI.State qualified as State
import UI.State qualified as UI (State)

draw :: UI.State -> [Brick.Widget UI.Resource]
draw s = case State.mode s of
  State.InMenu ->
    [ Brick.padAll 15 . Form.renderForm . MainMenu.form . State.mainMenu $ s
    ]
  State.InGame ->
    [ Brick.raw . Render.render . State.canvas $ s
    ]

event :: UI.State -> Brick.BrickEvent UI.Resource Game.Command -> Brick.EventM UI.Resource (Brick.Next UI.State)
event s evt = case evt of
  Brick.VtyEvent (Vty.EvKey key mods) -> do
    let given = Input.fromVty key mods
    State.broadcast s (fromMaybe Action.NoOp (given >>= Input.toAction))

    transition
      . State.sendMaybe s
      . fromMaybe Input.None
      $ given
  Brick.AppEvent (Action.Redraw canv) -> do
    Brick.continue (s & typed .~ canv)
  _ -> Brick.continue s
  where
    transition = maybe (Brick.halt s) Brick.continue

app :: Brick.App UI.State Game.Command UI.Resource
app =
  Brick.App
    { Brick.appDraw = draw,
      Brick.appChooseCursor = Brick.neverShowCursor,
      Brick.appHandleEvent = event,
      Brick.appAttrMap = const (Brick.AttrMap.attrMap Vty.defAttr []),
      Brick.appStartEvent = pure
    }
