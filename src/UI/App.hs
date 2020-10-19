{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
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
import Data.Position (components)
import Data.Maybe
import Data.Message
import Game.Action (Action, Dest (..))
import Game.Action qualified as Action
import Graphics.Vty qualified as Vty
import Optics
import UI.Attributes qualified as Attributes
import UI.Input qualified as Input
import UI.MainMenu qualified as MainMenu
import UI.Canvas qualified as Canvas
import UI.Resource qualified as Resource
import Data.Position qualified as Position
import UI.Resource qualified as UI (Resource)
import UI.Sidebar qualified as Sidebar
import UI.State (modeline, sidebar)
import UI.State qualified as State
import UI.State qualified as UI (State)
import UI.Widgets.Modeline (messages)
import UI.Widgets.Modeline qualified as Modeline

draw :: UI.State -> [Brick.Widget UI.Resource]
draw s =
  let curPos p = view components (p + offset)
      offset = Position.make 21 3 -- TODO: figure out how to query for the offset information of the sidebar
  in case s ^. State.mode of
    State.MainMenu ->
      [ MainMenu.render . view State.mainMenu $ s
      ]
    State.InGame ->
      [ Attributes.withStandard . Brick.border . Brick.vBox $
          [ Brick.hBox
              [ Brick.hLimit 25 . Brick.border . Sidebar.render . view State.sidebar $ s,
                Brick.border . Brick.padBottom Brick.Max . Canvas.render . view State.canvas $ s
              ],
            Brick.hBorder,
            Brick.vLimit 3 . Modeline.render . view modeline $ s
          ]
      ]
    State.Looking pos ->
      [ Brick.showCursor Resource.Look (pos^.to curPos%to Brick.Location) . Attributes.withStandard . Brick.border . Brick.vBox $
          [ Brick.hBox
              [ Brick.hLimit 25 . Brick.border . Sidebar.render . view State.sidebar $ s,
                Brick.border . Brick.padBottom Brick.Max . Brick.reportExtent Resource.Canvas . Render.render . view State.canvas $ s
              ],
            Brick.hBorder,
            Brick.vLimit 3 . Modeline.render . view modeline $ s
          ]
      ]

event :: UI.State -> Brick.BrickEvent UI.Resource (Action 'UI) -> Brick.EventM UI.Resource (Brick.Next UI.State)
event s evt = case evt of
  Brick.VtyEvent (Vty.EvKey key mods) -> do
    let inp = Input.fromVty key mods
    let action
          | has (State.mode % State._Looking) s = Action.NoOp
          | otherwise = fromMaybe Action.NoOp (inp >>= Input.toAction)



    liftIO
      . Broker.runBroker (error "no queue") (s ^. State.gamePort)
      . Broker.pushAction
      $ action

    maybe (shutdown s) Brick.continue
      . State.sendMaybe s
      . fromMaybe Input.None
      $ inp
  Brick.AppEvent cmd -> Brick.continue $ case cmd of
    Action.NoOp -> s
    Action.Redraw canv -> s & State.canvas .~ canv
    Action.Update inf -> s & sidebar % field @"info" .~ inf
    Action.Notify msg -> do
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

app :: Brick.App UI.State (Action 'UI) UI.Resource
app =
  Brick.App
    { Brick.appDraw = draw,
      Brick.appChooseCursor = Brick.showFirstCursor,
      Brick.appHandleEvent = event,
      Brick.appAttrMap = const (Brick.AttrMap.attrMap Vty.defAttr []),
      Brick.appStartEvent = pure
    }
