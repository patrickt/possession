{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module UI.App (app) where

import Brick qualified
import Brick.AttrMap qualified as Brick.AttrMap
import Brick.Forms qualified as Form
import Brick.Widgets.Center qualified as Brick
import Data.Maybe
import Game.Action qualified as Game (Command)
import Graphics.Vty qualified as Vty
import UI.Input qualified as Input
import UI.MainMenu qualified as MainMenu
import UI.Resource qualified as UI (Resource)
import UI.State qualified as State
import UI.State qualified as UI (State)

draw :: UI.State -> [Brick.Widget UI.Resource]
draw s = case State.mode s of
  State.InMenu -> [ Brick.padAll 15 . Form.renderForm . MainMenu.form . State.mainMenu $ s
                  ]

-- State.InGame -> [renderCanvas (State.canvas s)]

event :: UI.State -> Brick.BrickEvent UI.Resource Game.Command -> Brick.EventM UI.Resource (Brick.Next UI.State)
event s evt = case evt of
  Brick.VtyEvent (Vty.EvKey key _) -> do
    let given = Input.fromVty key
    maybe (pure ()) (State.broadcast s) (given >>= Input.toAction)

    transition
      . State.sendMaybe s
      . fromMaybe Input.None
      $ given
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
