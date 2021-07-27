{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module UI.App (app) where

import Brick qualified
import Brick.AttrMap qualified
import Control.Concurrent (killThread)
import Control.Effect.Broker qualified as Broker
import Control.Monad.IO.Class
import Data.Generics.Product
import Game.Action (Action, Dest (..))
import Game.Action qualified as Action
import Graphics.Vty qualified as Vty
import Optics
import UI.InGame (InGame)
import UI.Render
import UI.Resource qualified as UI (Resource)
import UI.Responder.Chain (castTo)
import UI.Responder.Chain qualified as Responder
import UI.Responder qualified as Responder
import UI.State qualified as State
import UI.State qualified as UI (State)
import UI.Widgets.Modeline qualified as Modeline

draw :: UI.State -> [Brick.Widget UI.Resource]
draw s = s ^. #responders % Responder.first % to renderMany

event :: UI.State -> Brick.BrickEvent UI.Resource (Action 'UI) -> Brick.EventM UI.Resource (Brick.Next UI.State)
event s evt = case evt of
  Brick.VtyEvent vty -> do
    let first = s ^. #responders % Responder.first
    let inp = Responder.translate vty first

    let go act = case act of
          Responder.Nil ->
            Brick.continue s
          a `Responder.Then` b -> go a *> go b
          Responder.Push r ->
            Brick.continue (s & #responders %~ Responder.push r)
          Responder.Pop -> do
            Brick.continue (s & #responders %~ Responder.pop)
          Responder.Update a ->
            Brick.continue (s & #responders % Responder.first .~ a)
          Responder.Broadcast it -> do
            liftIO
              . Broker.runBroker (s ^. #brokerage)
              . Broker.pushAction
              $ it
            Brick.continue s
          Responder.Terminate -> do
            liftIO (killThread (s ^. #gameThread))
            Brick.halt s

    go (Responder.onSend inp first)

  Brick.AppEvent cmd -> Brick.continue $ case cmd of
    Action.NoOp -> s
    Action.Redraw canv -> s & #responders %~ Responder.propagate @InGame (#canvas .~ canv)
    Action.Update info -> s & #responders %~ Responder.propagate @InGame (#sidebar % typed .~ info)
    Action.Notify msg -> do
      let lastMessage = State.firstResponder % castTo @InGame % #modeline % #messages % _last
      let previous = s ^? lastMessage % #contents
      let shouldCoalesce = previous == Just (msg ^. #contents)
      case (previous, shouldCoalesce) of
        (Just _, True) -> s & lastMessage % #times %~ (+ 1)
        _ -> s & #responders %~ Responder.propagate @InGame (#modeline %~ Modeline.update msg)
  _ -> Brick.continue s

app :: Brick.App UI.State (Action 'UI) UI.Resource
app =
  Brick.App
    { Brick.appDraw = draw,
      Brick.appChooseCursor = Brick.showFirstCursor,
      Brick.appHandleEvent = event,
      Brick.appAttrMap = const (Brick.AttrMap.attrMap Vty.defAttr []),
      Brick.appStartEvent = \s -> do
        liftIO
          . Broker.runBroker (s ^. #brokerage)
          . Broker.pushAction
          $ Action.NoOp
        pure s
    }
