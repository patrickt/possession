{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module UI.App (app) where

import Brick qualified
import Brick.AttrMap qualified as Brick.AttrMap
import Brick.Widgets.Border qualified as Brick
import Control.Concurrent (killThread)
import Control.Effect.Broker qualified as Broker
import Control.Monad.IO.Class
import Data.Generics.Product.Fields
import Data.Maybe
import Data.Message
import Game.Action qualified as Action
import Game.Command (Command)
import Game.Command qualified as Command
import Graphics.Vty qualified as Vty
import Optics
import UI.Attributes qualified as Attributes
import UI.Input qualified as Input
import UI.MainMenu qualified as MainMenu
import UI.Render qualified as Render
import UI.Resource qualified as UI (Resource)
import UI.Sidebar qualified as Sidebar
import UI.State (modeline, sidebar)
import UI.State qualified as State
import UI.State qualified as UI (State)
import UI.Widgets.Modeline (messages)
import UI.Widgets.Modeline qualified as Modeline

draw :: UI.State -> [Brick.Widget UI.Resource]
draw s = case s ^. State.mode of
  State.InMenu ->
    [ MainMenu.render . view State.mainMenu $ s
    ]
  State.InGame ->
    [ Attributes.withStandard . Brick.border . Brick.vBox $
        [ Brick.hBox
            [ Brick.hLimit 25 . Brick.border . Sidebar.render . view State.sidebar $ s,
              Brick.border . Brick.padBottom Brick.Max . Render.render . view State.canvas $ s
            ],
          Brick.hBorder,
          Brick.vLimit 3 . Modeline.render . view modeline $ s
        ]
    ]

event :: UI.State -> Brick.BrickEvent UI.Resource Command -> Brick.EventM UI.Resource (Brick.Next UI.State)
event s evt = case evt of
  Brick.VtyEvent (Vty.EvKey key mods) -> do
    let inp = Input.fromVty key mods
    let action = fromMaybe Action.NoOp (inp >>= Input.toAction)

    liftIO
      . Broker.runBroker (error "no queue") (s ^. State.gamePort)
      . Broker.pushAction
      $ action

    maybe (shutdown s) Brick.continue
      . State.sendMaybe s
      . fromMaybe Input.None
      $ inp
  Brick.AppEvent cmd -> Brick.continue $ case cmd of
    Command.Redraw canv -> s & State.canvas .~ canv
    Command.Update inf -> s & sidebar % field @"info" .~ inf
    Command.Notify msg -> do
      let previous = s ^? modeline % messages % _last % contents
      let shouldCoalesce = previous == Just (msg ^. contents)
      case (previous, shouldCoalesce) of
        (Just _, True) -> s & modeline % Modeline.messages % _last % times %~ succ
        _ -> s & modeline %~ Modeline.update msg
  _ -> Brick.continue s

shutdown :: UI.State -> Brick.EventM a (Brick.Next UI.State)
shutdown s = do
  liftIO (killThread (s ^. State.gameThread))
  Brick.halt s

app :: Brick.App UI.State Command UI.Resource
app =
  Brick.App
    { Brick.appDraw = draw,
      Brick.appChooseCursor = Brick.neverShowCursor,
      Brick.appHandleEvent = event,
      Brick.appAttrMap = const (Brick.AttrMap.attrMap Vty.defAttr []),
      Brick.appStartEvent = pure
    }
