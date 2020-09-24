{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module UI.App (app) where

import Brick qualified
import Brick.AttrMap qualified as Brick.AttrMap
import Brick.Forms qualified as Form
import Graphics.Vty qualified as Vty
import UI.Input qualified as Input
import UI.MainMenu qualified as MainMenu
import UI.Resource qualified as UI (Resource)
import UI.State qualified as State
import UI.State qualified as UI (State)
import Data.Maybe

draw :: UI.State -> [Brick.Widget UI.Resource]
draw s = case State.mode s of
  State.InMenu -> [Form.renderForm (MainMenu.form (State.mainMenu s))]

event :: UI.State -> Brick.BrickEvent UI.Resource evt -> Brick.EventM UI.Resource (Brick.Next UI.State)
event s evt = case evt of
  Brick.VtyEvent (Vty.EvKey key _) ->
    transition
      . State.sendMaybe s
      . fromMaybe Input.None
      . Input.fromVty
      $ key
  _ -> Brick.continue s
  where
    transition = maybe (Brick.halt s) Brick.continue

app :: Brick.App UI.State e UI.Resource
app =
  Brick.App
    { Brick.appDraw = draw,
      Brick.appChooseCursor = Brick.neverShowCursor,
      Brick.appHandleEvent = event,
      Brick.appAttrMap = const (Brick.AttrMap.attrMap Vty.defAttr []),
      Brick.appStartEvent = pure
    }
